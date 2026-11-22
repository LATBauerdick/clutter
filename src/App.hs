{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module App (
  startApp,
  app,
  appWith,
)
where

import qualified Data.ByteString.Lazy as BL
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)

import Control.Exception (try)
import Control.Monad (foldM)
import qualified Data.Set as Set
import qualified Data.Map.Strict as M
import Data.Text as T (breakOn, isInfixOf, isSuffixOf, stripPrefix, takeWhile, unlines, unpack)
import qualified Network.HTTP.Client as HC
import Data.Text.IO as TIO (writeFile)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as V
import Env (
  envGetTag,
  envInit,
  envRefreshPlays,
  envTidalConnect,
  envTidalReload,
  envUpdate,
  envUpdateAlbum,
  envUpdateMenuParams,
  envUpdateSort,
 )
import Listened (appendPlay, listenedDir, listenedRows, playAlbum, playEvent)
import Related (Related (..), noRelated, related)
import Tracks (AlbumTracks, albumTracks)
import qualified Lucid as L
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Relude
import Render (renderAlbumJournal, renderAlbumText, renderAlbumView, renderAlbumsView, renderApp)
import Servant
import System.Directory (doesFileExist, makeAbsolute)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hPutStrLn)
import System.Process (rawSystem, readProcess, readProcessWithExitCode)
import Types (
  Album (..),
  AlbumFacts (..),
  AlbumKey (..),
  AppM,
  Discogs (..),
  Env (..),
  EnvR (..),
  MenuParams (..),
  PlayEvent (..),
  envGetDiscogs,
  envGetEnvr,
 )

import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecode, encode, object, withObject, (.!=), (.:?), (.=))

newtype WantsJ = WantsJ {wnotes :: Text} deriving (Show, Generic)
instance FromJSON WantsJ where
  parseJSON = withObject "wants" $ \o -> do
    wnotes_ <- o .:? "notes" .!= ""
    pure $ WantsJ wnotes_

data HTML

newtype RawHtml = RawHtml {unRaw :: BL.ByteString}

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

------------- Clutter API defined in Servant

type API0 =
  "album"
    :> Capture "aid" AlbumKey
    :> Get '[HTML] RawHtml

type API1 =
  "albums"
    :> Capture "list" Text
    :> QueryParam "sortBy" Text
    :> QueryParam "sortOrder" Text
    :> QueryParams "focus" Text
    :> Get '[HTML] RawHtml

type API2 =
  "provider"
    :> "discogs"
    :> QueryParam "token" Text
    :> QueryParam "username" Text
    :> QueryParam "nreleases" Int -- update last n releases, all if null
    :> QueryParam "release" Int -- get another release into the database
    :> Get '[HTML] RawHtml

type API3 =
  "provider"
    :> "tidal"
    :> QueryParam "nalbums" Int -- update last n tidal albums, all if null
    :> Get '[HTML] RawHtml

type API4 =
  "api"
    :> "albumq"
    :> Capture "aid" AlbumKey
    :> Get '[JSON] AlbumJ

type API5 =
  "api"
    :> "albumsq"
    :> Capture "list" Text
    :> QueryParam "sortBy" Text
    :> QueryParam "sortOrder" Text
    :> QueryParams "focus" Text
    :> Get '[JSON] AlbumsJ

type API6 =
  "api"
    :> "paramsq"
    :> "all"
    :> Get '[JSON] ParamsJ

type API7 =
  "api"
    :> "req"
    :> QueryParam "event" Text
    :> Get '[JSON] ReqJ

type API8 =
  "api"
    :> "albump"
    :> Capture "aid" AlbumKey
    :> Get '[JSON] AlbumJ

type API9 =
  "app"
    :> Get '[HTML] RawHtml

-- the play log, one row per play: "all" or a year
type API10 =
  "api"
    :> "listenedq"
    :> Capture "year" Text
    :> QueryParam "sortBy" Text
    :> QueryParam "sortOrder" Text
    :> QueryParams "focus" Text
    :> Get '[JSON] ListenedJ

-- play a Tidal album on the Bluesound Node through its BluOS API (port 11000)
-- mode: replace (default) | now | next | last
type API11 =
  "api"
    :> "nodeplay"
    :> Capture "tid" Int
    :> QueryParam "mode" Text
    :> Get '[JSON] ReqJ

-- Node transport: toggle (pause/resume) | pause | play | stop
type API12 =
  "api"
    :> "nodectl"
    :> Capture "cmd" Text
    :> Get '[JSON] ReqJ

-- the Node's player name from /SyncStatus (e.g. "lblue") in reqInfo
type API13 =
  "api"
    :> "nodeinfo"
    :> Get '[JSON] ReqJ

-- the album page's details (docs/album-page-plan.md, phase 1): facts from the stores and
-- the album strips. Asked for when an album is opened, so that Album stays small in lists.
type API14 =
  "api"
    :> "albumxq"
    :> Capture "aid" AlbumKey
    :> Get '[JSON] AlbumXJ

data AlbumXJ = AlbumXJ
  { xid :: AlbumKey
  , xfacts :: AlbumFacts
  , xrelated :: Related
  , xloc :: Maybe (Text, Int) -- where the physical album is: location list and position in it
  }
  deriving (Show, Generic)
instance ToJSON AlbumXJ

-- tracks, credits and review of an album (docs/album-page-plan.md, phase 2): fetched from
-- Discogs (a physical release) or Tidal (a stream) when first asked for, then from the
-- per-album cache; refresh=1 refetches, source=tidal asks for a release's Tidal version
type API15 =
  "api"
    :> "albumtq"
    :> Capture "aid" AlbumKey
    :> QueryParam "refresh" Text
    :> QueryParam "source" Text -- "tidal": the Tidal version of a Discogs release
    :> Get '[JSON] AlbumTJ

data AlbumTJ = AlbumTJ
  { tjId :: AlbumKey
  , tjTracks :: Maybe AlbumTracks
  , tjError :: Text -- why there are none, "" otherwise
  }
  deriving (Show, Generic)
instance ToJSON AlbumTJ

-- remove a stream album from clutter (docs/album-page-plan.md): marked "dropped" in the Tidal
-- album store, by the script that owns the store, and left out from then on; its plays stay
type API16 =
  "api"
    :> "albumdrop"
    :> Capture "aid" AlbumKey
    :> Get '[JSON] ReqJ

data AlbumJ = AlbumJ
  { aid :: AlbumKey
  , album :: Maybe Album
  }
  deriving (Eq, Show, Generic)
instance ToJSON AlbumJ

data AlbumsJ = AlbumsJ
  { listName :: Text
  , -- , lalbums :: [Album]
    lalbums :: [(Album, Maybe (Text, Int))]
  }
  deriving (Eq, Show, Generic)
instance ToJSON AlbumsJ

-- AlbumsJ plus, row by row, the time of that play (same JSON keys as AlbumsJ)
data ListenedJ = ListenedJ
  { ljName :: Text
  , ljAlbums :: [(Album, Maybe (Text, Int))]
  , ljPlayed :: [Text]
  }
  deriving (Eq, Show, Generic)
instance ToJSON ListenedJ where
  toJSON l = object ["listName" .= ljName l, "lalbums" .= ljAlbums l, "lplayed" .= ljPlayed l]

data ParamsJ = ParamsJ
  { timeStamp :: Text
  , params :: MenuParams
  }
  deriving (Eq, Show, Generic)
instance ToJSON ParamsJ

data ReqJ = ReqJ
  { reqTime :: Text
  , reqInfo :: Text
  , reqResult :: Int
  }
  deriving (Eq, Show, Generic)
instance ToJSON ReqJ

type ClutterAPI =
  API0
    :<|> API1
    :<|> API2
    :<|> API3
    :<|> API4
    :<|> API5
    :<|> API6
    :<|> API7
    :<|> API8
    :<|> API9
    :<|> API10
    :<|> API11
    :<|> API12
    :<|> API13
    :<|> API14
    :<|> API15
    :<|> API16
    :<|> Raw

clutterAPI :: Proxy ClutterAPI
clutterAPI = Proxy

-- add "Middleware", see
-- https://mmhaskell.com/blog/2018/10/8/stuck-in-the-middle-adding-middleware-to-a-servant-server
-- where we modify the response header such that it includes CORS header Access-Control-Allow-Origin: *
--
addAllOriginsMiddleware :: Application -> Application
addAllOriginsMiddleware baseApp req responseFunc = baseApp req (responseFunc . addOriginsAllowed)

-- addAllOriginsMiddleware baseApp = \req responseFunc -> baseApp req (responseFunc . addOriginsAllowed)

addOriginsAllowed :: Response -> Response
addOriginsAllowed =
  mapResponseHeaders $
    (:) ("Access-Control-Allow-Origin", "*")

---
--- keep a rebuilt frontend bundle (or stylesheet) from being served out of a stale
--- browser cache. With no Cache-Control at all the browser is free to invent a
--- freshness lifetime from the file's age (~10% of it), so weeks after a build a
--- plain reload can still run the old index.js. "no-cache" does not disable the
--- cache, it only forces revalidation -- and the static file server answers
--- If-Modified-Since with a 304, so the cost is one conditional request.
---
addNoCacheMiddleware :: Application -> Application
addNoCacheMiddleware baseApp req responseFunc =
  baseApp req (responseFunc . if isAssetPath (pathInfo req) then addNoCache else id)

isAssetPath :: [Text] -> Bool
isAssetPath ps = case reverse ps of
  (p : _) -> any (`T.isSuffixOf` p) [".js", ".css"]
  [] -> False

addNoCache :: Response -> Response
addNoCache =
  mapResponseHeaders $
    (:) ("Cache-Control", "no-cache")

-- (:) ("Content-Security-Policy", "navigate-to * ")

-- type AppM = ReaderT Env Handler defined in Types.hs
nt :: Env -> (ReaderT Env) Handler a -> Handler a
nt env x = x `runReaderT` env

app :: Env -> Application
app = appWith "static"

appWith :: FilePath -> Env -> Application
appWith sd env = addAllOriginsMiddleware . addNoCacheMiddleware $ serve clutterAPI (hoistServer clutterAPI (`runReaderT` env) (clutterServer sd))

clutterServer :: FilePath -> ServerT ClutterAPI AppM
clutterServer sd =
  serveAlbum
    :<|> serveAlbums
    :<|> serveDiscogs
    :<|> serveTidal
    :<|> serveAlbumq
    :<|> serveAlbumsq
    :<|> serveParamsq
    :<|> serveReq
    :<|> serveAlbump
    :<|> serveApp
    :<|> serveListenedq
    :<|> serveNodeplay
    :<|> serveNodectl
    :<|> serveNodeinfo
    :<|> serveAlbumxq
    :<|> serveAlbumtq
    :<|> serveAlbumdrop
    :<|> serveDirectoryFileServer sd
 where
  decodeListQuery :: Text -> Maybe Text -> Maybe Text -> [Text] -> AppM (V.Vector AlbumKey)
  decodeListQuery ln msb mso fs = do
    -- listName sortBy sortOrder focusItems
    env <- ask
    liftIO $ print ("-------decodeListQuery " :: Text, ln, msb, mso, fs)

    -- get ids from either list or tags (#tag)
    aids' <- case T.stripPrefix "#" ln of
      Just tag -> envGetTag tag
      Nothing -> V.toList <$> getList env ln
    ---------------------------------------------------------------------------------
    -- filter with focus if any
    aidset <- focusFilter fs (Set.fromList aids')
    let aids = V.mapMaybe (\i -> if i `Set.member` aidset then Just i else Nothing) . V.fromList $ aids'
    ---------------------------------------------------------------------------------
    envr <- envUpdateSort msb mso
    let EnvR am _ _ _ _ sn so _ _ _ = envr
    let doSort = getSort env am sn
    pure . doSort so $ aids

  -- narrow a set of album ids by focus items: #tag keeps, -#tag drops
  focusFilter :: [Text] -> Set.Set AlbumKey -> AppM (Set.Set AlbumKey)
  focusFilter fs aidset' =
    foldM
      ( \s (t, p) ->
          ( if p
              then Set.intersection s
              else Set.difference s
          )
            . Set.fromList
            <$> envGetTag t
      )
      aidset'
      . map
        ( \t ->
            ( fromMaybe t (T.stripPrefix "-" t)
            , isNothing (T.stripPrefix "-" t)
            )
        )
      . mapMaybe (T.stripPrefix "#")
      $ fs

  -- every play of a year ("all": of every year). Sorted like the other lists: Default is
  -- the list's own order, newest play first; Artist, Title, Added sort the rows by their
  -- album (stable, so plays of one album stay newest first); sortOrder=Desc reverses.
  -- A play of an album clutter does not hold gets one made up from the play, and is
  -- dropped by any focus filter
  serveListenedq :: Text -> Maybe Text -> Maybe Text -> [Text] -> AppM ListenedJ
  serveListenedq y msb mso fs = do
    liftIO . putTextLn $ "-------serveListenedq " <> y
    env <- ask
    es <- readIORef (playsR env)
    envr <- envGetEnvr
    let my = if y == "all" then Nothing else Just y
        rows = listenedRows my es (albums envr)
        shown = [(e, a) | (e, Just a) <- rows]
    keep <- focusFilter fs (Set.fromList (albumID . snd <$> shown))
    let rows' =
          [ (e, fromMaybe (playAlbum e) ma)
          | (e, ma) <- rows
          , null fs || maybe False ((`Set.member` keep) . albumID) ma
          ]
        sorted = case msb of
          Just "Artist" -> sortOn (albumArtist . snd) rows'
          Just "Title" -> sortOn (albumTitle . snd) rows'
          Just "Added" -> sortOn (Down . albumAdded . snd) rows'
          _ -> rows'
        ordered = if mso == Just "Desc" then reverse sorted else sorted
    pure
      ListenedJ
        { ljName = if y == "all" then "Listened" else "Listened " <> y
        , ljAlbums = [(a, albumID a `M.lookup` locs envr) | (_, a) <- ordered]
        , ljPlayed = peTs . fst <$> ordered
        }

  serveAlbumsq ::
    Text ->
    Maybe Text ->
    Maybe Text ->
    [Text] ->
    AppM AlbumsJ
  serveAlbumsq ln msb mso fs = do
    liftIO . putTextLn $ "-------serveAlbumsq " <> ln
    aids <- decodeListQuery ln msb mso fs
    envr <- envGetEnvr
    -- gl <- asks getList
    -- aids <- gl ln
    -- let la = mapMaybe (`M.lookup` albums envr) . V.toList $ aids
    let la =
          mapMaybe
            ( \aid' -> case (aid' `M.lookup` albums envr, aid' `M.lookup` locs envr) of
                (Nothing, _) -> Nothing
                (Just a, b) -> Just (a, b)
            )
            . V.toList
            $ aids
    liftIO . putTextLn $ "-------serveAlbumsq: first 3 elements from [(album, locs)] of list " <> ln
    liftIO . print . take 3 $ la
    let asj = AlbumsJ{listName = ln, lalbums = la}
    pure asj

  serveAlbumq :: AlbumKey -> AppM AlbumJ
  serveAlbumq i = do
    liftIO $ print ("-------serveAlbumq " :: Text, i)
    ma <- envUpdateAlbum i
    let aj = case ma of
          Just a -> AlbumJ{aid = i, album = Just a}
          _ -> AlbumJ{aid = i, album = Nothing}
    pure aj

  serveAlbumxq :: AlbumKey -> AppM AlbumXJ
  serveAlbumxq i = do
    am <- asks albumsR >>= readIORef
    facts <- asks albumFactsR >>= readIORef
    lcs <- asks locsR >>= readIORef
    pure $ case M.lookup i am of
      Just a -> AlbumXJ{xid = i, xfacts = facts a, xrelated = related am a, xloc = M.lookup i lcs}
      -- not in the collection (an album made up from a play): no details
      Nothing -> AlbumXJ{xid = i, xfacts = AlbumFacts Nothing Nothing Nothing Nothing Nothing Nothing False [] Nothing, xrelated = noRelated, xloc = Nothing}

  serveAlbumtq :: AlbumKey -> Maybe Text -> Maybe Text -> AppM AlbumTJ
  serveAlbumtq i mrefresh msource = do
    am <- asks albumsR >>= readIORef
    case M.lookup i am of
      Nothing -> pure AlbumTJ{tjId = i, tjTracks = Nothing, tjError = "not in the collection"}
      Just a -> do
        r <- liftIO $ albumTracks (isJust mrefresh) (msource == Just "tidal") a
        pure $ case r of
          Right t -> AlbumTJ{tjId = i, tjTracks = Just t, tjError = ""}
          Left err -> AlbumTJ{tjId = i, tjTracks = Nothing, tjError = err}

  serveAlbumdrop :: AlbumKey -> AppM ReqJ
  serveAlbumdrop i = do
    now <- liftIO getZonedTime
    let dtl = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" now
        answer ok info = ReqJ{reqTime = dtl, reqInfo = info, reqResult = if ok then 1 else 0}
    am <- asks albumsR >>= readIORef
    case (i, M.lookup i am) of
      (T n, Just a) -> do
        ran <- liftIO . try $ readProcessWithExitCode "python3" ["scripts/update-tidal-albums.py", "--drop", show n] ""
        case ran :: Either SomeException (ExitCode, String, String) of
          Right (ExitSuccess, _, _) -> do
            _ <- envTidalReload
            liftIO $ hPutStrLn stderr ("removed from clutter: " <> show i)
            pure $ answer True ("Removed “" <> albumTitle a <> "” from clutter")
          _ -> pure $ answer False "Could not remove the album (the store was not changed)"
      (T _, Nothing) -> pure $ answer False "Not in clutter"
      -- a Discogs release leaves clutter when it leaves the Discogs collection
      _ -> pure $ answer False "Only a stream album can be removed here"

  serveParamsq :: AppM ParamsJ
  serveParamsq = do
    liftIO $ print ("-------serveParamsq " :: Text)
    now <- liftIO getZonedTime -- `debugId`
    let dtl = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" now
    ms <- envUpdateMenuParams
    let sj =
          ParamsJ
            { timeStamp = dtl
            , params = ms
            }
    pure sj

  serveReq :: Maybe Text -> AppM ReqJ
  serveReq r = do
    liftIO $ print ("-------serveReq " :: Text, r)
    now <- liftIO getZonedTime
    let dtl = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" now
    let sj =
          ReqJ
            { reqTime = dtl
            , reqInfo = fromMaybe "" r
            , reqResult = 1
            }
    case r of
      Just "update" -> envUpdate Nothing Nothing 20 >> pure sj
      -- default is to update last 20 release records
      -- Tidal: the favourites and the Save for later list, without a restart. update-json
      -- --tidal refreshes the token and brings the album store up to date; its output is
      -- dropped unread. Then the store is taken in again.
      Just "tidal" -> do
        ran <- liftIO . try $ readProcessWithExitCode "./update-json" ["--tidal"] ""
        let failed = case ran :: Either SomeException (ExitCode, String, String) of
              Right (ExitSuccess, _, _) -> Nothing
              Right (ExitFailure c, _, _) -> Just ("update-json --tidal failed (" <> show c <> ")")
              Left _ -> Just "update-json could not be run"
        new <- envTidalReload
        liftIO $ hPutStrLn stderr ("update from Tidal: " <> show new <> " new albums" <> maybe "" (\e -> "; " <> toString e) failed)
        pure $ case failed of
          Nothing -> sj{reqInfo = show new <> (if new == 1 then " new album" else " new albums") <> " from Tidal"}
          Just e -> sj{reqInfo = e, reqResult = 0}
      _ -> liftIO (print r) >> pure sj

  serveNodeplay :: Int -> Maybe Text -> AppM ReqJ
  serveNodeplay tid mm = do
    liftIO $ print ("-------serveNodeplay " :: Text, tid, mm)
    now <- liftIO getZonedTime
    let dtl = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" now
    base <- liftIO bluosBase
    (ok, info) <- liftIO $ bluosAdd base tid (fromMaybe "replace" mm)
    liftIO $ hPutStrLn stderr ("nodeplay " <> show tid <> ": " <> toString info)
    pure ReqJ{reqTime = dtl, reqInfo = info, reqResult = if ok then 1 else 0}

  serveNodectl :: Text -> AppM ReqJ
  serveNodectl cmd = do
    now <- liftIO getZonedTime
    let dtl = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" now
    base <- liftIO bluosBase
    (ok, info) <- liftIO $ bluosCtl base cmd
    liftIO $ hPutStrLn stderr ("nodectl " <> toString cmd <> ": " <> toString info)
    pure ReqJ{reqTime = dtl, reqInfo = info, reqResult = if ok then 1 else 0}

  serveNodeinfo :: AppM ReqJ
  serveNodeinfo = do
    now <- liftIO getZonedTime
    let dtl = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" now
    base <- liftIO bluosBase
    r <- liftIO $ bluosGet base "/SyncStatus"
    let (ok, info) = case r of
          Left err -> (False, err)
          -- <SyncStatus … name="lblue" model="N130" modelName="NODE" …>
          Right b -> case T.stripPrefix " name=\"" . snd $ T.breakOn " name=\"" b of
            Just rest | T.takeWhile (/= '"') rest /= "" -> (True, T.takeWhile (/= '"') rest)
            _ -> (False, "Node: no name in /SyncStatus")
    liftIO $ hPutStrLn stderr ("nodeinfo: " <> toString info)
    pure ReqJ{reqTime = dtl, reqInfo = info, reqResult = if ok then 1 else 0}

  bluosBase :: IO String
  bluosBase = fromMaybe "http://lblue.local:11000" <$> lookupEnv "CLUTTER_BLUOS"

  serveAlbump :: AlbumKey -> AppM AlbumJ
  serveAlbump i = do
    liftIO $ print ("-------serveAlbump " :: Text, i)
    -- a Tidal or Apple album is played as it is: refreshing it through
    -- envUpdateAlbum would re-read Discogs folders and lists
    known <- asks albumsR >>= readIORef
    ma <- case M.lookup i known of
      Just a | not (isDiscogs (albumID a)) -> pure (Just a)
      _ -> envUpdateAlbum i
    maybe (print ("no Album" :: Text)) updateAlbumsPlayed ma
    -- the album again, now with this play in its dates
    ma' <- case ma of
      Just a -> Just . fromMaybe a . M.lookup i <$> (asks albumsR >>= readIORef)
      Nothing -> pure Nothing
    let aj = case ma' of
          Just a -> AlbumJ{aid = i, album = Just a}
          _ -> AlbumJ{aid = i, album = Nothing}
    pure aj

  serveApp :: AppM RawHtml
  serveApp = do
    now <- liftIO getZonedTime -- `debugId`
    liftIO $ print $ ("-------serveApp " :: Text) <> show now
    html <- renderApp "Clutter Prototype"
    pure . RawHtml $ L.renderBS html

  serveAlbum :: AlbumKey -> AppM RawHtml
  serveAlbum a = do
    liftIO $ print ("-------serveAlbum " :: Text, a)
    now <- liftIO getZonedTime -- `debugId`
    ma <- envUpdateAlbum a
    html <- renderAlbumView ma now
    pure . RawHtml $ L.renderBS html

  serveAlbums ::
    Text ->
    Maybe Text ->
    Maybe Text ->
    [Text] ->
    AppM RawHtml
  serveAlbums ln msb mso fs = do
    aids <- decodeListQuery ln msb mso fs
    html <- renderAlbumsView ln fs aids
    pure . RawHtml $ L.renderBS html

  serveDiscogs :: Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> AppM RawHtml
  serveDiscogs token username nreleases release = do
    liftIO $ print ("------Updating from Discogs" :: Text, token, username, nreleases, release)
    envUpdate token username (fromMaybe 0 nreleases)
    let ln :: Text; ln = "Discogs"
    gl <- asks getList
    aids <- gl ln
    html <- renderAlbumsView ln [] aids
    pure . RawHtml $ L.renderBS html

  serveTidal :: Maybe Int -> AppM RawHtml
  serveTidal nalbums = do
    liftIO $ print ("------Connecting to Tidal" :: Text)
    _ <- envTidalConnect (fromMaybe 0 nalbums)
    let ln = "Tidal"
    gl <- asks getList
    aids <- gl ln
    html <- renderAlbumsView ln [] aids
    pure . RawHtml $ L.renderBS html

-- BluOS: queue a Tidal album by its Tidal album id and play it. The parameters are
-- those of the "Play now" entry in the player's own /Services menu (Tidal-Album);
-- tested live on the Node 2026-11-21, see ~/Notes/Claude/2026-11-21-bluos-api-for-clutter.md
-- Returns (ok, message); never throws.
bluosAdd :: String -> Int -> Text -> IO (Bool, Text)
bluosAdd base tid mode = do
  let ps :: String
      ps = case mode of
        "now" -> "&playnow=1&clear=0&shuffle=0&where=nextAlbum" -- after the current album
        "next" -> "&playnow=-1&where=nextAlbum" -- queue, don't play
        "last" -> "&playnow=-1&where=last"
        _ -> "&playnow=1&clear=1&where=nextAlbum" -- replace the queue
  r <- bluosGet base ("/Add?service=Tidal&albumid=" <> show tid <> ps)
  pure $ case r of
    Left err -> (False, err)
    Right b ->
      -- reply is <playlist length="6" id="362" …/> or <error …><message>…</message></error>
      let len = maybe "?" (T.takeWhile (/= '"')) . T.stripPrefix "length=\"" . snd $ T.breakOn "length=\"" b
       in if "<error" `T.isInfixOf` b || not ("<playlist" `T.isInfixOf` b)
            then (False, "Node refused: " <> b)
            else (True, "Node: " <> (if mode `elem` ["next", "last"] then "queued, " else "playing, ") <> len <> " tracks in queue")

-- BluOS transport: toggle (pause/resume), pause, play, stop.
-- The player answers <state>pause</state> (or play/stop/stream).
bluosCtl :: String -> Text -> IO (Bool, Text)
bluosCtl base cmd = case cmdPath of
  Nothing -> pure (False, "Node: unknown command " <> cmd)
  Just p -> do
    r <- bluosGet base p
    pure $ case r of
      Left err -> (False, err)
      Right b ->
        let st = T.takeWhile (/= '<') . snd . T.breakOn ">" . snd $ T.breakOn "<state" b
         in if "<error" `T.isInfixOf` b
              then (False, "Node refused: " <> b)
              else (True, "Node: " <> (if st == "" then b else fromMaybe st (T.stripPrefix ">" st)))
 where
  cmdPath :: Maybe String
  cmdPath = case cmd of
    "toggle" -> Just "/Pause?toggle=1"
    "pause" -> Just "/Pause"
    "play" -> Just "/Play"
    "stop" -> Just "/Stop"
    _ -> Nothing

-- one GET to the player; Left is a short, human-readable failure
bluosGet :: String -> String -> IO (Either Text Text)
bluosGet base path = do
  r <- try $ do
    mgr <- HC.newManager HC.defaultManagerSettings{HC.managerResponseTimeout = HC.responseTimeoutMicro 15000000}
    req <- HC.parseRequest (base <> path)
    HC.responseBody <$> HC.httpLbs req mgr
  pure $ case r of
    Left (HC.HttpExceptionRequest _ c) -> Left ("Node not reachable at " <> toText base <> ": " <> show c)
    Left e -> Left ("Node: bad request " <> show e)
    Right body -> Right (decodeUtf8 body)

isDiscogs :: AlbumKey -> Bool
isDiscogs k = case k of
  D _ -> True
  _ -> False

updateAlbumsPlayed :: Album -> AppM ()
updateAlbumsPlayed a = do
  now <- liftIO getZonedTime
  let key = albumID a
  --
  -- clutter's own play log comes first, for every kind of album
  ldir <- liftIO listenedDir
  let pe = playEvent now a
  logged <- liftIO . try $ appendPlay ldir pe
  liftIO . hPutStrLn stderr $ case logged of
    Right fn -> "listened: " <> show key <> " -> " <> fn
    Left (err :: SomeException) -> "ERROR listened: could not log " <> show key <> " in " <> ldir <> ": " <> show err
  -- and show up in the Listened lists and play counts without a restart
  when (isRight logged) $ do
    asks playsR >>= \r -> modifyIORef' r (<> [pe])
    envRefreshPlays
  --
  -- add a Note in Obsidian
  -- assumes that the Obsidian Vault is linked to `./AlbumsPlayed`
  let ts = renderAlbumText a now

  let fn = fromMaybe "!!error!!" $ viaNonEmpty head ts
  let pathName = "AlbumsPlayed/" <> fn <> ".md"
  putTextLn ("-----updating albums played: " <> pathName)
  liftIO $ TIO.writeFile (T.unpack pathName) (T.unlines $ drop 1 ts)
  --
  -- enter a journal entry in Day One
  -- enable Day One CLI w/ `sudo bash /Applications/Day\ One.app/Contents/Resources/install_cli.sh`
  -- make sure Journal named "Albums Played" does already exist
  --  let ns = renderAlbumDayOne a
  let js = map T.unpack $ renderAlbumJournal a now
  putTextLn "-----add entry into Day One Journal Albums Played, file in AlbumsPlayed"
  print js
  let iurl = fromMaybe "!!error!!" $ viaNonEmpty head js
  -- The Day One CLI (symlinked from /usr/local/bin into the app bundle since
  -- 2026-09-18) runs App-Sandboxed and can only read its own containers, so
  -- the cover must be staged inside the Day One app-group container, not /tmp.
  home <- fromMaybe "/tmp" <$> lookupEnv "HOME"
  let itmp = home <> "/Library/Group Containers/5U8NS4GX82.dayoneapp2/__ac.jpg"
  -- -f: an HTTP error body (Discogs "You are making requests too quickly")
  -- must not be left behind masquerading as a JPEG.
  -- --retry: opening an album fires a burst of Discogs API calls just before
  -- this, and i.discogs.com then answers 429 (seen 2026-11-19); curl treats
  -- 429 as transient, backs off (honouring Retry-After) and tries again.
  exitCode <- liftIO $ rawSystem "curl" ["-fsSL", "--retry", "5", "--retry-max-time", "60", iurl, "-o", itmp]
  unless (exitCode == ExitSuccess) $
    putTextLn $
      "ERROR getting cover img: curl failed with exit code: " <> show exitCode
  --
  -- only a Discogs release has a Discogs id: a Tidal or Apple id sent to
  -- /wants/ or /releases/ would 404, or hit an unrelated release
  dc <- envGetDiscogs
  case (key, dc) of
    (D rid, DiscogsSession ttok tu) -> updateDiscogsPlayed ttok tu rid a
    (D _, _) -> putTextLn "-----no Discogs session, Discogs not updated"
    _ -> liftIO . hPutStrLn stderr $ "listened: " <> show key <> " is not a Discogs release, no Discogs calls"

  putTextLn ""
  -- only attach the cover if the download actually succeeded
  let attachArgs = if exitCode == ExitSuccess then ["-a", itmp] else []
  let args = ["-j", "Albums Played"] <> attachArgs <> ["--", "new"] <> drop 1 js
  exitCode' <- liftIO $ rawSystem "dayone" args
  unless (exitCode' == ExitSuccess) $
    putTextLn $
      "ERROR adding to DayOne.app: failed with exit code: " <> show exitCode'
  print args

  pure ()

-- Discogs-side bookkeeping for a play of a Discogs release:
-- mark the collection field 7 "Play Count"; the Want list stamp is
-- replaced by the play log and only written with CLUTTER_DISCOGS_WANTS=1
updateDiscogsPlayed :: Text -> Text -> Int -> Album -> AppM ()
updateDiscogsPlayed ttok tu rid a = do
  now <- liftIO getZonedTime
  let taid = show rid :: Text
  let tinst = show $ albumInst a
  let auth = "Authorization: Discogs token=" <> unpack ttok
      wantsURL = "https://api.discogs.com/users/" <> unpack tu <> "/wants/" <> unpack taid

  wantsFlag <- lookupEnv "CLUTTER_DISCOGS_WANTS"
  when (wantsFlag == Just "1") $ do
    putTextLn "-----add to Discogs Want List (CLUTTER_DISCOGS_WANTS=1)"
    -- see if this release is on Want list, and get current notes
    res <-
      ( eitherDecode . TLE.encodeUtf8 . TL.pack
          <$> liftIO
            ( readProcess
                "curl"
                [ "-X"
                , "PUT"
                , "-H"
                , auth
                , "-H"
                , "Content-Type: application/json; charset=utf-8"
                , wantsURL
                ]
                ""
            )
      ) ::
        AppM (Either String WantsJ)
    case res of
      Left err -> putTextLn $ "Error decoding Want List: " <> show err
      Right wants -> print wants -- pure ()
    let tno = case res of
          Left _ -> ""
          Right wants -> wnotes wants

    let td = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" now
        tn = case tno of
          "" -> td
          _ -> td <> "\n" <> tno
    exitCode3 <-
      liftIO $
        rawSystem
          "curl"
          [ "-X"
          , "POST"
          , "-H"
          , auth
          , "-H"
          , "Content-Type: application/json; charset=utf-8"
          , "--data"
          , jsonBody ["notes" .= tn]
          , wantsURL
          ]
    unless (exitCode3 == ExitSuccess) $
      putTextLn $
        "ERROR adding date to Discogs Want List: failed with exit code: " <> show exitCode3
  --
  -- mark the physical copy as played: field 7 "Play Count" (a dropdown 1..6, Many) is set
  -- to 1 when empty and otherwise left alone -- clutter's play log does the counting
  let tfield = "7"
      tpls = "1" :: Text
  if albumInst a == 0
    then putTextLn "-----not in the Discogs collection, Play Count not marked"
    else if albumPlays a > 0
    then putTextLn "-----Play Count already marked on Discogs"
    else do
      exitCode4 <-
        liftIO $
          rawSystem
            "curl"
            [ "-X"
            , "POST"
            , "-H"
            , auth
            , "-H"
            , "Content-Type: application/json; charset=utf-8"
            , "--data"
            , jsonBody ["value" .= tpls]
            , "https://api.discogs.com/users/" <> unpack tu <> "/collection/folders/0/releases/" <> unpack taid <> "/instances/" <> unpack tinst <> "/fields/" <> unpack tfield
            ]
      unless (exitCode4 == ExitSuccess) $
        putTextLn $
          "ERROR marking Discogs Play Count: failed with exit code: " <> show exitCode4
 where
  jsonBody = TL.unpack . TLE.decodeUtf8 . encode . object

-- init env from files (AppM not yet available) and run app
startApp :: Int -> Bool -> IO ()
startApp p c = do
  sd <- resolveStaticDir
  ldir <- listenedDir
  hPutStrLn stderr $ "logging plays to " <> ldir
  env <- envInit c
  run p (appWith sd env)

---
--- the static assets (index.js, style.css, icons) live next to the sources, not in
--- the installed package, and were looked up relative to the process working
--- directory -- so a restart from anywhere else served a page with no script and no
--- stylesheet, with nothing in the log to say why. Resolve the directory once at
--- startup, make it absolute so a later chdir cannot break it, allow an override,
--- and complain loudly if the bundle is not where we think it is.
---
resolveStaticDir :: IO FilePath
resolveStaticDir = do
  sd <- makeAbsolute . fromMaybe "static" =<< lookupEnv "CLUTTER_STATIC"
  haveBundle <- doesFileExist (sd </> "index.js")
  --- stderr, not stdout: stdout is block-buffered once redirected to a log file,
  --- so a warning printed there can sit unflushed for the life of the process.
  if haveBundle
    then hPutStrLn stderr $ "serving static assets from " <> sd
    else
      hPutStrLn stderr $
        "WARNING: no index.js under "
          <> sd
          <> " -- the web UI will not load. Start clutter from the clutter"
          <> " source directory, or point CLUTTER_STATIC at its static/ directory."
  pure sd
