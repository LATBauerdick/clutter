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
import qualified Data.IntSet as Set
import qualified Data.Map.Strict as M
import Data.Text as T (isSuffixOf, stripPrefix, unlines, unpack)
import Data.Text.IO as TIO (writeFile)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as V
import Env (
  envGetTag,
  envInit,
  envRefreshPlays,
  envTidalConnect,
  envUpdate,
  envUpdateAlbum,
  envUpdateMenuParams,
  envUpdateSort,
 )
import Listened (AlbumKey (..), albumKey, appendPlay, listenedDir, listenedRows, playAlbum, playEvent)
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
import System.Process (rawSystem, readProcess)
import Types (
  Album (..),
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
    :> Capture "aid" Int
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
    :> Capture "aid" Int
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
    :> Capture "aid" Int
    :> Get '[JSON] AlbumJ

type API9 =
  "app"
    :> Get '[HTML] RawHtml

-- the play log, one row per play: "all" or a year
type API10 =
  "api"
    :> "listenedq"
    :> Capture "year" Text
    :> QueryParam "sortOrder" Text
    :> QueryParams "focus" Text
    :> Get '[JSON] ListenedJ

data AlbumJ = AlbumJ
  { aid :: Int
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
    :<|> serveDirectoryFileServer sd
 where
  decodeListQuery :: Text -> Maybe Text -> Maybe Text -> [Text] -> AppM (V.Vector Int)
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
  focusFilter :: [Text] -> Set.IntSet -> AppM Set.IntSet
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

  -- every play of a year ("all": of every year), newest first (sortOrder=Asc: oldest first);
  -- a play of an album clutter does not hold gets one made up from the play (albumID 0),
  -- and is dropped by any focus filter
  serveListenedq :: Text -> Maybe Text -> [Text] -> AppM ListenedJ
  serveListenedq y mso fs = do
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
        ordered = if mso == Just "Asc" then reverse rows' else rows'
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

  serveAlbumq :: Int -> AppM AlbumJ
  serveAlbumq i = do
    liftIO $ print ("-------serveAlbumq " :: Text, i)
    ma <- envUpdateAlbum i
    let aj = case ma of
          Just a -> AlbumJ{aid = i, album = Just a}
          _ -> AlbumJ{aid = 0, album = Nothing}
    pure aj

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
      Just "update" -> envUpdate Nothing Nothing 20
      -- default is to update last 20 release records
      _ -> liftIO $ print r
    pure sj

  serveAlbump :: Int -> AppM AlbumJ
  serveAlbump i = do
    liftIO $ print ("-------serveAlbump " :: Text, i)
    -- a Tidal or Apple album is played as it is: refreshing it through
    -- envUpdateAlbum would re-read Discogs folders and lists
    known <- asks albumsR >>= readIORef
    ma <- case M.lookup i known of
      Just a | not (isDiscogs (albumKey a)) -> pure (Just a)
      _ -> envUpdateAlbum i
    maybe (print ("no Album" :: Text)) updateAlbumsPlayed ma
    -- the album again, now with this play in its dates
    ma' <- case ma of
      Just a -> Just . fromMaybe a . M.lookup i <$> (asks albumsR >>= readIORef)
      Nothing -> pure Nothing
    let aj = case ma' of
          Just a -> AlbumJ{aid = i, album = Just a}
          _ -> AlbumJ{aid = 0, album = Nothing}
    pure aj

  serveApp :: AppM RawHtml
  serveApp = do
    now <- liftIO getZonedTime -- `debugId`
    liftIO $ print $ ("-------serveApp " :: Text) <> show now
    html <- renderApp "Clutter Prototype"
    pure . RawHtml $ L.renderBS html

  serveAlbum :: Int -> AppM RawHtml
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

isDiscogs :: AlbumKey -> Bool
isDiscogs k = case k of
  KDiscogs _ -> True
  _ -> False

updateAlbumsPlayed :: Album -> AppM ()
updateAlbumsPlayed a = do
  now <- liftIO getZonedTime
  let key = albumKey a
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
  -- must not be left behind masquerading as a JPEG
  exitCode <- liftIO $ rawSystem "curl" ["-fsSL", iurl, "-o", itmp]
  unless (exitCode == ExitSuccess) $
    putTextLn $
      "ERROR getting cover img: curl failed with exit code: " <> show exitCode
  --
  -- only a Discogs release has a Discogs id: a Tidal or Apple id sent to
  -- /wants/ or /releases/ would 404, or hit an unrelated release
  dc <- envGetDiscogs
  case (key, dc) of
    (KDiscogs _, DiscogsSession ttok tu) -> updateDiscogsPlayed ttok tu a
    (KDiscogs _, _) -> putTextLn "-----no Discogs session, Discogs not updated"
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
-- increment the collection field 7 "Plays"; the Want list stamp is
-- replaced by the play log and only written with CLUTTER_DISCOGS_WANTS=1
updateDiscogsPlayed :: Text -> Text -> Album -> AppM ()
updateDiscogsPlayed ttok tu a = do
  now <- liftIO getZonedTime
  let taid = show $ albumID a
  let tinst = show $ albumInst a
  let tpls = case albumPlays a of
        6 -> "" -- ????LATB should be a configuration parameter
        _ -> show $ 1 + albumPlays a :: Text
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
  -- increment played count, if the release is in the collection
  let tfield = "7" -- ????LATB should be a configuration parameter
  if albumInst a == 0
    then putTextLn "-----not in the Discogs collection, Plays field not updated"
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
          "ERROR incrementing Discogs Plays field: failed with exit code: " <> show exitCode4
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
