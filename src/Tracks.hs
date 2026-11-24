{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- Track list, credits and review for the album page, fetched on demand the first time a page
-- is opened and then kept per album in ~/Sync/clutter/album-details/<key>.json
-- (CLUTTER_ALBUM_DETAILS overrides). Tidal when the album has a Tidal id (tracks, credits,
-- review), else Discogs for a Discogs release (tracks, credits). Where Tidal has no review, the
-- lead of the Wikipedia article takes its place: the album's page if it has one, else that of
-- its first artist with one. Tokens come from tok.json in
-- the working directory, read at fetch time because the Tidal token lives only 4 hours; no
-- part of a token is ever logged or returned.
module Tracks (Track (..), Credit (..), AlbumTracks (..), albumTracks, cacheDir) where

import Control.Exception (SomeAsyncException, onException, throwIO, try)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, eitherDecode, encode, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Key (Key)
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.ByteString as B
import qualified Data.Char as Ch
import Data.List (zipWith3)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import Network.HTTP.Client (
  HttpException (..),
  HttpExceptionContent (..),
  Manager,
  checkResponse,
  httpLbs,
  managerResponseTimeout,
  parseRequest,
  requestHeaders,
  responseBody,
  responseStatus,
  responseTimeoutMicro,
  setQueryString,
 )
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Relude
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName, (</>))
import System.IO (hClose, hPutStrLn, openTempFileWithDefaultPermissions)
import System.Process (readProcessWithExitCode)
import Types (Album (..), AlbumKey (..), ArtistRef (..), keyText)
import Wikidata (displayName)

data Track = Track
  { tkDisc :: Int -- volume / disc number, 1 if unknown
  , tkPos :: Text -- position as shown: "1", "2" … (Tidal) or Discogs' "A1", "B2"
  , tkWork :: Maybe Text -- the work this track is a movement of (classical)
  , tkTitle :: Text -- the track title; the movement part only when tkWork is set
  , tkSecs :: Maybe Int -- duration in seconds
  , tkArtists :: [Text] -- Tidal track artists; [] for Discogs
  }
  deriving (Show, Generic)
instance ToJSON Track
instance FromJSON Track

data Credit = Credit
  { crRole :: Text -- "Producer", "Composer", "Piano" …
  , crNames :: [Text]
  }
  deriving (Show, Generic)
instance ToJSON Credit
instance FromJSON Credit

data AlbumTracks = AlbumTracks
  { atSource :: Text -- "tidal" | "discogs"
  , atFetched :: Text -- local time of the fetch, "2026-11-22T14:05"
  , atTracks :: [Track]
  , atCredits :: [Credit]
  , atReview :: Maybe Text -- plain text, paragraphs separated by "\n\n"
  , atReviewSource :: Maybe Text -- e.g. "TiVo"; "Wikipedia" for an article's lead
  , atReviewUrl :: Maybe Text -- the Wikipedia article the text is from
  , atReviewAbout :: Maybe Text -- the artist the text is about; Nothing: about the album
  , atAboutChecked :: Maybe Bool -- Wikipedia was asked (found or not): not again until a refresh
  }
  deriving (Show, Generic)
instance ToJSON AlbumTracks
instance FromJSON AlbumTracks

-- entry point ---------------------------------------------------------------------------

-- FromDiscogs carries the linked Tidal album, if any, for the review
data Source = FromTidal Int | FromDiscogs Int (Maybe Int)

-- A physical release (a Discogs key) shows the tracks of that release, from Discogs, unless
-- its Tidal version is asked for; a stream shows Tidal's.
sourceOf :: Bool -> Album -> Maybe Source
sourceOf wantTidal a = case (albumID a, tidalTwin) of
  (T n, _) -> Just (FromTidal n)
  (D _, Just t) | wantTidal -> Just (FromTidal t)
  (D n, t) -> Just (FromDiscogs n t)
  (_, Just t) -> Just (FromTidal t)
  _ -> Nothing
 where
  tidalTwin = albumTidal a >>= readMaybe . toString . T.strip

-- refresh = True skips the cache. Left is a short human-readable reason. Never throws
-- (asynchronous exceptions excepted: a cancelled request thread must stay cancellable).
-- wantTidal: for a Discogs release with a linked Tidal album, the Tidal version's tracks.
albumTracks :: Bool -> Bool -> Album -> IO (Either Text AlbumTracks)
albumTracks refresh wantTidal a =
  tryAll (albumTracks' refresh wantTidal a) >>= \case
    Right r -> pure r
    Left e
      | isAsync e -> throwIO e
      | otherwise -> do
          -- the exception text may carry a URL or path, so it is not passed on
          logLine (keyText (albumID a) <> ": failed unexpectedly")
          pure (Left "tracks: unexpected error")

albumTracks' :: Bool -> Bool -> Album -> IO (Either Text AlbumTracks)
albumTracks' refresh wantTidal a = do
  dir <- cacheDir
  let key = keyText (albumID a)
      msrc = sourceOf wantTidal a
      -- the Tidal version of a Discogs release is cached next to the release's own tracks
      suffix = case (albumID a, msrc) of
        (D _, Just (FromTidal _)) -> "-tidal"
        _ -> ""
      fn = dir </> (toString key <> suffix <> ".json")
  cached <- if refresh then pure Nothing else readCache fn
  case cached of
    Just at -> Right <$> withAbout dir fn a at
    Nothing -> case msrc of
      Nothing -> pure (Left "no source for tracks")
      Just src -> do
        mgr <- newTlsManagerWith tlsManagerSettings{managerResponseTimeout = responseTimeoutMicro 20000000}
        let srcName = case src of
              FromTidal _ -> "tidal"
              FromDiscogs _ _ -> "discogs"
        r <- case src of
          FromTidal n -> fetchTidal mgr n
          FromDiscogs n twin ->
            fetchDiscogs mgr n >>= \case
              -- the review is about the album, whatever the pressing: from the Tidal twin
              Right (ts, cs, _, _) | Just t <- twin -> do
                rv <- fetchTidalReview mgr t
                pure (Right (ts, cs, fst <$> rv, rv >>= snd))
              other -> pure other
        case r of
          Left err -> do
            logLine (key <> ": " <> srcName <> " failed: " <> err)
            pure (Left err)
          Right (ts, cs, rv, rvs) -> do
            now <- toText . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" <$> getZonedTime
            let at = AlbumTracks srcName now ts cs rv rvs Nothing Nothing Nothing
            logLine (key <> ": " <> srcName <> ", " <> show (length ts) <> " tracks")
            writeCache dir fn at
            Right <$> withAbout dir fn a at

type Fetched = ([Track], [Credit], Maybe Text, Maybe Text)

logLine :: Text -> IO ()
logLine t = hPutStrLn stderr ("album tracks " <> toString t)

tryAll :: IO a -> IO (Either SomeException a)
tryAll = try

isAsync :: SomeException -> Bool
isAsync e = isJust (fromException e :: Maybe SomeAsyncException)

-- cache -----------------------------------------------------------------------------------

cacheDir :: IO FilePath
cacheDir =
  lookupEnv "CLUTTER_ALBUM_DETAILS" >>= \case
    Just d | not (null d) -> pure d
    _ -> do
      home <- fromMaybe "." <$> lookupEnv "HOME"
      pure $ home </> "Sync" </> "clutter" </> "album-details"

-- unreadable or undecodable means absent: it is refetched and overwritten
readCache :: FilePath -> IO (Maybe AlbumTracks)
readCache fn = do
  r <- tryAll $ doesFileExist fn >>= \ok -> if ok then Just <$> readFileBS fn else pure Nothing
  pure $ case r of
    Right (Just bs) -> rightToMaybe (eitherDecode (fromStrict bs))
    _ -> Nothing

-- temp file in the same directory, then rename, so a reader never sees half a file; a failed
-- write is logged, the fetched data is still returned
writeCache :: FilePath -> FilePath -> AlbumTracks -> IO ()
writeCache dir fn at = do
  r <- tryAll $ do
    createDirectoryIfMissing True dir
    (tmp, h) <- openTempFileWithDefaultPermissions dir (takeFileName fn <> ".tmp")
    hClose h
    (writeFileLBS tmp (encode at) >> renameFile tmp fn)
      `onException` tryAll (removeFile tmp)
  whenLeft_ r $ \_ -> hPutStrLn stderr ("album tracks: could not write cache " <> fn)

-- credentials -----------------------------------------------------------------------------

-- a key of tok.json, read fresh each time; nothing about the file is ever reported
readToken :: Key -> IO (Maybe Text)
readToken k = do
  r <- tryAll (readFileBS "tok.json")
  pure $ case r of
    Right bs
      | Right v <- eitherDecode (fromStrict bs) ->
          mfilter (not . T.null) (parseMaybe (withObject "tok.json" (.: k)) (v :: Value))
    _ -> Nothing

-- ./tidalauth refreshes tok.json; its output can echo tokens, so it is discarded unread
runTidalAuth :: IO Bool
runTidalAuth =
  tryAll (readProcessWithExitCode "./tidalauth" [] "") <&> \case
    Right (ExitSuccess, _, _) -> True
    _ -> False

-- HTTP ------------------------------------------------------------------------------------

type Query = [(ByteString, Maybe ByteString)]

-- status and body; a non-2xx status is a normal result, only transport failures are Left
httpGet :: Manager -> Text -> String -> Query -> [(ByteString, ByteString)] -> IO (Either Text (Int, LByteString))
httpGet mgr svc url q hs = do
  r <- try $ do
    req0 <- parseRequest url
    let req =
          setQueryString
            q
            req0
              { requestHeaders = first fromString' <$> hs
              , checkResponse = \_ _ -> pure ()
              }
    resp <- httpLbs req mgr
    pure (fromEnum (responseStatus resp), responseBody resp)
  pure (first (httpErr svc) r)
 where
  fromString' = fromString . decodeUtf8

-- named by service only: the exception's own text includes the request with its headers
httpErr :: Text -> HttpException -> Text
httpErr svc = \case
  HttpExceptionRequest _ ResponseTimeout -> svc <> ": timed out"
  HttpExceptionRequest _ ConnectionTimeout -> svc <> ": timed out"
  HttpExceptionRequest _ (ConnectionFailure _) -> svc <> ": connection failed"
  _ -> svc <> ": request failed"

-- Tidal -----------------------------------------------------------------------------------

data TSession = TSession
  { tsMgr :: Manager
  , tsToken :: IORef (Maybe Text)
  , tsRefreshed :: IORef Bool -- ./tidalauth runs at most once per fetch
  }

notAuthorised :: Either Text a
notAuthorised = Left "Tidal: not authorised (token refresh failed)"

-- GET below https://api.tidal.com/v1/; a 401 refreshes the token and retries once
tidalGet :: TSession -> String -> Query -> IO (Either Text (Int, LByteString))
tidalGet s path q =
  readIORef (tsToken s) >>= \case
    Nothing -> pure (Left "Tidal: no token in tok.json")
    Just tok ->
      send tok >>= \case
        Right (401, _) -> do
          done <- readIORef (tsRefreshed s)
          writeIORef (tsRefreshed s) True
          ok <- if done then pure False else runTidalAuth
          tok' <- if ok then readToken "tdt" else pure Nothing
          writeIORef (tsToken s) tok'
          case tok' of
            Nothing -> pure notAuthorised
            Just t ->
              send t <&> \case
                Right (200, b) -> Right (200, b)
                Right _ -> notAuthorised
                Left e -> Left e
        other -> pure other
 where
  send :: Text -> IO (Either Text (Int, LByteString))
  send tok =
    httpGet
      (tsMgr s)
      "Tidal"
      ("https://api.tidal.com/v1/" <> path)
      q
      [("Authorization", "Bearer " <> encodeUtf8 tok)]

fetchTidal :: Manager -> Int -> IO (Either Text Fetched)
fetchTidal mgr n = do
  s <- TSession mgr <$> (readToken "tdt" >>= newIORef) <*> newIORef False
  let albumPath = "albums/" <> show n
      -- offset over raw items (videos included), since the total counts them too
      pages :: Int -> Int -> [(Track, [Credit])] -> IO (Either Text [(Track, [Credit])])
      pages k off acc =
        tidalGet
          s
          (albumPath <> "/items/credits")
          [ ("limit", Just "100")
          , ("offset", Just (show off))
          , ("includeContributors", Just "true")
          , ("replace", Just "true")
          , ("countryCode", Just "US")
          ]
          >>= \case
            Left e -> pure (Left e)
            Right (200, b) -> case rightToMaybe (eitherDecode b) >>= tidalPage of
              Nothing -> pure (Left "Tidal: unexpected response")
              Just (total, got, ts) ->
                let acc' = acc <> ts
                    off' = off + got
                 in if got == 0 || off' >= total || k >= 50
                      then pure (Right acc')
                      else pages (k + 1) off' acc'
            Right (404, _) -> pure (Left "Tidal: album not found")
            Right (st, _) -> pure (Left ("Tidal: HTTP " <> show st))
  pages (1 :: Int) 0 [] >>= \case
    Left e -> pure (Left e)
    Right items -> do
      rv <- tidalReviewOf s n
      pure $
        Right
          ( groupWorks (fst <$> items)
          , mergeCredits (concatMap snd items)
          , fst <$> rv
          , rv >>= snd
          )

-- 404 is the normal "no review"; any other failure here costs only the review
tidalReviewOf :: TSession -> Int -> IO (Maybe (Text, Maybe Text))
tidalReviewOf s n =
  tidalGet s ("albums/" <> show n <> "/review") [("countryCode", Just "US")] <&> \case
    Right (200, b) -> rightToMaybe (eitherDecode b) >>= tidalReview
    _ -> Nothing

-- the review alone, for a Discogs release with a linked Tidal album
fetchTidalReview :: Manager -> Int -> IO (Maybe (Text, Maybe Text))
fetchTidalReview mgr n = do
  s <- TSession mgr <$> (readToken "tdt" >>= newIORef) <*> newIORef False
  tidalReviewOf s n

-- (totalNumberOfItems, items on this page, the tracks among them with their credits);
-- videos and malformed items are dropped one by one
tidalPage :: Value -> Maybe (Int, Int, [(Track, [Credit])])
tidalPage = parseMaybe $ withObject "tidal items" $ \o -> do
  items <- o .:? "items" .!= []
  total <- o .:? "totalNumberOfItems" .!= length items
  pure (total, length items, mapMaybe (parseMaybe tidalItem) (items :: [Value]))

tidalItem :: Value -> Parser (Track, [Credit])
tidalItem = withObject "tidal item" $ \o -> do
  typ <- o .: "type"
  guard (typ == ("track" :: Text))
  i <- o .: "item"
  t <- withObject "tidal track" track i
  cs <- o .:? "credits" .!= []
  pure (t, mapMaybe (parseMaybe tidalCredit) (cs :: [Value]))
 where
  track t = do
    title <- t .:? "title" .!= ""
    version <- t .:? "version"
    secs <- t .:? "duration"
    pos <- t .:? "trackNumber"
    disc <- t .:? "volumeNumber" .!= 1
    artists <- t .:? "artists" .!= []
    pure
      Track
        { tkDisc = disc
        , tkPos = maybe "" (show :: Int -> Text) pos
        , tkWork = Nothing
        , tkTitle = withVersion title version
        , tkSecs = secs
        , tkArtists = mapMaybe (parseMaybe nameOf) (artists :: [Value])
        }
  withVersion title = \case
    Just v | not (T.null (T.strip v)) -> title <> " (" <> T.strip v <> ")"
    _ -> title

nameOf :: Value -> Parser Text
nameOf = withObject "named" (.: "name")

tidalCredit :: Value -> Parser Credit
tidalCredit = withObject "tidal credit" $ \o -> do
  role <- o .: "type"
  cs <- o .:? "contributors" .!= []
  pure (Credit role (mapMaybe (parseMaybe nameOf) (cs :: [Value])))

-- (text, source)
tidalReview :: Value -> Maybe (Text, Maybe Text)
tidalReview v = do
  (txt, src) <- parseMaybe (withObject "tidal review" $ \o -> (,) <$> o .:? "text" .!= "" <*> o .:? "source") v
  let clean = cleanReview txt
  guard (not (T.null clean))
  pure (clean, mfilter (not . T.null) src)

-- drop [wimpLink …]…[/wimpLink] tags keeping the linked text; runs of <br/> become paragraphs
cleanReview :: Text -> Text
cleanReview =
  T.intercalate "\n\n"
    . filter (not . T.null)
    . fmap T.strip
    . T.splitOn "<br/>"
    . T.replace "<br>" "<br/>"
    . T.replace "<br />" "<br/>"
    . stripLinks

stripLinks :: Text -> Text
stripLinks s = case T.breakOn "[" s of
  (pre, rest)
    | T.null rest -> pre
    | "[wimpLink" `T.isPrefixOf` rest || "[/wimpLink" `T.isPrefixOf` rest ->
        pre <> stripLinks (T.drop 1 (T.dropWhile (/= ']') rest))
    | otherwise -> pre <> "[" <> stripLinks (T.drop 1 rest)

-- "Piano Sonata No. 20 …, D. 959: I. Allegro" -> work and movement, but only where two or
-- more consecutive tracks share the work, so a lone title with a colon stays whole
groupWorks :: [Track] -> [Track]
groupWorks ts = zipWith3 pick (Nothing : ws) ts (drop 1 ws <> [Nothing])
 where
  ws = fmap fst . workSplit . tkTitle <$> ts
  pick prev t next = case workSplit (tkTitle t) of
    Just (w, rest) | prev == Just w || next == Just w -> t{tkWork = Just w, tkTitle = rest}
    _ -> t

workSplit :: Text -> Maybe (Text, Text)
workSplit title = case T.breakOn ": " title of
  (w, rest)
    | not (T.null rest)
    , not (T.null (T.strip w))
    , m <- T.strip (T.drop 2 rest)
    , not (T.null m) ->
        Just (T.strip w, m)
  _ -> Nothing

-- per-track credits as album credits: roles in first-seen order, names likewise, no repeats
mergeCredits :: [Credit] -> [Credit]
mergeCredits cs =
  [ Credit r names
  | r <- ordNub (filter (not . T.null) (crRole <$> cs))
  , let names = ordNub (filter (not . T.null) (concat [crNames c | c <- cs, crRole c == r]))
  , not (null names)
  ]

-- Wikipedia -------------------------------------------------------------------------------

-- the article to take the about text from, and the artist it is about (Nothing: the album)
-- Among the artists a performer comes before a composer (LATB, 2026-11-23): on a classical
-- release the composer is usually listed first, and the page should be about who plays.
-- Composers are those the credits name as such ("Composed By" on Discogs, "Composer" on Tidal);
-- if every linked artist is one (a band playing its own songs), the first is taken.
wikiTarget :: [Credit] -> Album -> Maybe (Text, Maybe Text)
wikiTarget credits a =
  ((\u -> (u, Nothing)) <$> albumWiki a)
    <|> listToMaybe (filter (not . isComposer . snd) linked <> linked)
 where
  linked = [(u, Just (displayName (arName r))) | r <- albumArtists a, Just u <- [arWiki r]]
  composers =
    [T.toCaseFold n | c <- credits, "compos" `T.isInfixOf` T.toLower (crRole c), n <- crNames c]
  isComposer = maybe False ((`elem` composers) . T.toCaseFold)

-- An album without a review gets the lead of its Wikipedia article, or of its first artist's
-- that has one (links from wikidata.json, already on the Album). Asked once per cache file,
-- also for files written before this existed; a transport failure is tried again next time.
withAbout :: FilePath -> FilePath -> Album -> AlbumTracks -> IO AlbumTracks
withAbout dir fn a at
  | isJust (atReview at) || atAboutChecked at == Just True = pure at
  | otherwise = case wikiTarget (atCredits at) a of
      Nothing -> pure at
      Just (url, about) -> do
        mgr <- newTlsManagerWith tlsManagerSettings{managerResponseTimeout = responseTimeoutMicro 10000000}
        fetchWikiLead mgr url >>= \case
          Left err -> logLine (keyText (albumID a) <> ": " <> err) $> at
          Right mtxt -> do
            let at' =
                  at
                    { atReview = mtxt
                    , atReviewSource = "Wikipedia" <$ mtxt
                    , atReviewUrl = url <$ mtxt
                    , atReviewAbout = mtxt *> about
                    , atAboutChecked = Just True
                    }
            logLine (keyText (albumID a) <> ": Wikipedia lead " <> maybe "not usable" (const "found") mtxt)
            writeCache dir fn at'
            pure at'

-- The lead section of a Wikipedia article as plain text, paragraphs separated by "\n\n".
-- Right Nothing: no such page, a disambiguation page, or too little text to be worth showing.
fetchWikiLead :: Manager -> Text -> IO (Either Text (Maybe Text))
fetchWikiLead mgr url = case wikiHostTitle url of
  Nothing -> pure (Right Nothing)
  Just (host, title) ->
    httpGet
      mgr
      "Wikipedia"
      ("https://" <> toString host <> "/w/api.php")
      [ ("action", Just "query")
      , ("prop", Just "extracts|pageprops")
      , ("ppprop", Just "disambiguation")
      , ("exintro", Just "1")
      , ("explaintext", Just "1")
      , ("redirects", Just "1")
      , ("format", Just "json")
      , ("formatversion", Just "2")
      , ("titles", Just title)
      ]
      [("User-Agent", "clutter/0.1 (personal music library)")]
      <&> \case
        Left e -> Left e
        Right (200, b) -> Right (rightToMaybe (eitherDecode b) >>= wikiLead)
        Right (st, _) -> Left ("Wikipedia: HTTP " <> show st)

-- "https://en.wikipedia.org/wiki/Attack_%26_Release" -> host and the title's bytes; only
-- Wikipedia hosts, so a stray link in the store cannot send the server elsewhere
wikiHostTitle :: Text -> Maybe (Text, ByteString)
wikiHostTitle url = do
  rest <- T.stripPrefix "https://" url
  let (host, path) = T.breakOn "/" rest
  guard (".wikipedia.org" `T.isSuffixOf` host && T.all (\c -> Ch.isAlphaNum c || c == '.' || c == '-') host)
  title <- T.stripPrefix "/wiki/" path
  let t = pctDecode (encodeUtf8 (T.takeWhile (/= '#') title))
  guard (not (B.null t))
  pure (host, t)

pctDecode :: ByteString -> ByteString
pctDecode = B.pack . go . B.unpack
 where
  go (37 : h : l : r) | Just x <- hex h, Just y <- hex l = (x * 16 + y) : go r
  go (c : r) = c : go r
  go [] = []
  hex w
    | w >= 48 && w <= 57 = Just (w - 48)
    | w >= 65 && w <= 70 = Just (w - 55)
    | w >= 97 && w <= 102 = Just (w - 87)
    | otherwise = Nothing

wikiLead :: Value -> Maybe Text
wikiLead v = do
  (missing, disambig, extract) <- flip parseMaybe v $ withObject "wikipedia" $ \o -> do
    q <- o .: "query"
    ps <- q .: "pages"
    case (ps :: [Value]) of
      p : _ -> flip (withObject "page") p $ \pg -> do
        missing <- pg .:? "missing" .!= False
        props <- pg .:? "pageprops"
        dis <- maybe (pure Nothing) (.:? "disambiguation") props
        extract <- pg .:? "extract" .!= ""
        pure (missing, isJust (dis :: Maybe Text), extract)
      [] -> fail "no page"
  guard (not missing && not disambig)
  let clean = T.intercalate "\n\n" (filter (not . T.null) (T.strip <$> T.lines extract))
  guard (T.length clean >= 80)
  pure (tidyLead clean)

-- An article about a person opens with "Name ( VAHG-nər; German: [ˈvɪlhɛlm …]; 22 May 1813 –
-- 13 February 1883) was …": of that first bracket keep only the parts with dates or a birth
-- name, since plain text has lost the pronunciation markup. Anything else is left as it is.
tidyLead :: Text -> Text
tidyLead t = case T.breakOn "(" t of
  (pre, rest)
    | not (T.null rest)
    , T.length pre < 120
    , Just (inner, post) <- closing (0 :: Int) "" (toString (T.drop 1 rest)) ->
        let segs = T.strip <$> T.splitOn ";" inner
            keep = filter (\s -> T.any Ch.isDigit s || "born" `T.isPrefixOf` s) segs
         in if length keep == length segs
              then t
              else
                if null keep
                  then T.stripEnd pre <> post
                  else pre <> "(" <> T.intercalate "; " keep <> ")" <> post
  _ -> t
 where
  -- the text up to the bracket that closes the first one, and what follows it
  closing _ _ [] = Nothing
  closing d acc (c : cs)
    | c == ')' && d == 0 = Just (toText (reverse acc), toText cs)
    | c == ')' = closing (d - 1) (c : acc) cs
    | c == '(' = closing (d + 1) (c : acc) cs
    | otherwise = closing d (c : acc) cs

-- Discogs ---------------------------------------------------------------------------------

fetchDiscogs :: Manager -> Int -> IO (Either Text Fetched)
fetchDiscogs mgr n = do
  tok <- readToken "tok"
  -- releases are public, so a missing token only costs the higher rate limit
  let auth = maybe [] (\t -> [("Authorization", "Discogs token=" <> encodeUtf8 t)]) tok
  httpGet mgr "Discogs" ("https://api.discogs.com/releases/" <> show n) [] (("User-Agent", "clutter/0.1") : auth)
    <&> \case
      Left e -> Left e
      Right (200, b) -> case rightToMaybe (eitherDecode b) >>= discogsRelease of
        Just (ts, cs) -> Right (ts, cs, Nothing, Nothing)
        Nothing -> Left "Discogs: unexpected response"
      Right (404, _) -> Left "Discogs: release not found"
      Right (401, _) -> Left "Discogs: not authorised"
      Right (429, _) -> Left "Discogs: rate limited, try again in a minute"
      Right (st, _) -> Left ("Discogs: HTTP " <> show st)

data DEntry = DEntry
  { deType :: Text
  , dePos :: Text
  , deTitle :: Text
  , deDur :: Text
  , deExtra :: [Value]
  , deSubs :: [DEntry]
  }

dEntry :: Value -> Parser DEntry
dEntry = withObject "discogs track" $ \o -> do
  subs <- o .:? "sub_tracks" .!= []
  DEntry
    <$> o .:? "type_" .!= "track"
    <*> (T.strip <$> o .:? "position" .!= "")
    <*> (T.strip <$> o .:? "title" .!= "")
    <*> (T.strip <$> o .:? "duration" .!= "")
    <*> o .:? "extraartists" .!= []
    <*> pure (mapMaybe (parseMaybe dEntry) (subs :: [Value]))

discogsRelease :: Value -> Maybe ([Track], [Credit])
discogsRelease = parseMaybe $ withObject "discogs release" $ \o -> do
  tl <- o .:? "tracklist" .!= []
  ex <- o .:? "extraartists" .!= []
  let entries = mapMaybe (parseMaybe dEntry) (tl :: [Value])
      allExtra e = deExtra e <> concatMap allExtra (deSubs e)
  pure (dTracks Nothing entries, mergeCredits (concatMap dCredits (ex <> concatMap allExtra entries)))

-- a heading names the work of the tracks after it, an index carries its movements as
-- sub_tracks; either ends the previous heading's reach
dTracks :: Maybe Text -> [DEntry] -> [Track]
dTracks _ [] = []
dTracks w (e : es) = case deType e of
  "heading" -> dTracks (nonEmpty' (deTitle e)) es
  "index" -> [dTrack (nonEmpty' (deTitle e)) s | s <- deSubs e, deType s == "track"] <> dTracks Nothing es
  "track" -> dTrack w e : dTracks w es
  _ -> dTracks w es
 where
  nonEmpty' t = if T.null t then Nothing else Just t

dTrack :: Maybe Text -> DEntry -> Track
dTrack w e =
  let (disc, pos) = discPos (dePos e)
   in Track disc pos w (deTitle e) (duration (deDur e)) []

-- "2-3" / "2.3" is disc 2, track 3; "A1" and plain "3" are disc 1 as written
discPos :: Text -> (Int, Text)
discPos p = case T.findIndex (\c -> c == '-' || c == '.') p of
  Just i
    | (d, rest) <- T.splitAt i p
    , not (T.null d)
    , T.all Ch.isDigit d
    , pos <- T.drop 1 rest
    , not (T.null pos)
    , Just n <- readMaybe (toString d) ->
        (n, pos)
  _ -> (1, p)

-- "3:44" or "1:02:03" to seconds; "" or anything odd is unknown
duration :: Text -> Maybe Int
duration t = case T.splitOn ":" t of
  parts
    | length parts >= 2 && length parts <= 3 ->
        foldl' (\acc x -> acc * 60 + x) 0 <$> traverse (readMaybe . toString . T.strip) parts
  _ -> Nothing

-- one {name, role} entry; role may hold several roles, "Other [Contributor], Vocals [Additional]"
dCredits :: Value -> [Credit]
dCredits v = case parseMaybe (withObject "discogs credit" $ \o -> (,) <$> o .: "name" <*> o .:? "role" .!= "") v of
  Just (name, roles)
    | n <- T.strip (displayName (T.strip name))
    , not (T.null n) ->
        [Credit r [n] | r <- splitRoles roles]
  _ -> []

-- split on commas outside brackets; the bracketed detail stays with its role
splitRoles :: Text -> [Text]
splitRoles = filter (not . T.null) . fmap T.strip . go (0 :: Int) [] . toString
 where
  go _ acc [] = [toText (reverse acc)]
  go d acc (c : cs)
    | c == '[' = go (d + 1) (c : acc) cs
    | c == ']' = go (max 0 (d - 1)) (c : acc) cs
    | c == ',' && d == 0 = toText (reverse acc) : go d [] cs
    | otherwise = go d (c : acc) cs
