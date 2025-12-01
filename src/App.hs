{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module App (
  startApp,
  app,
)
where

import qualified Data.ByteString.Lazy as BL
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)

import Control.Monad (foldM)
import qualified Data.IntSet as Set
import qualified Data.Map.Strict as M
import Data.Text as T (replace, stripPrefix, unlines, unpack)
import Data.Text.IO as TIO (writeFile)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as V
import Env (
  envGetTag,
  envInit,
  envTidalConnect,
  envUpdate,
  envUpdateAlbum,
  envUpdateMenuParams,
  envUpdateSort,
 )
import qualified Lucid as L
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Relude
import Render (renderAlbumJournal, renderAlbumText, renderAlbumView, renderAlbumsView, renderApp)
import Servant
import System.Exit (ExitCode (..))
import System.Process (rawSystem, readProcess)
import Types (
  Album (..),
  AppM,
  Discogs (..),
  Env (..),
  EnvR (..),
  MenuParams (..),
  envGetDiscogs,
  envGetEnvr,
 )

import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecode, withObject, (.!=), (.:?))

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

-- (:) ("Content-Security-Policy", "navigate-to * ")

-- type AppM = ReaderT Env Handler defined in Types.hs
nt :: Env -> (ReaderT Env) Handler a -> Handler a
nt env x = x `runReaderT` env

app :: Env -> Application
app env = addAllOriginsMiddleware $ serve clutterAPI (hoistServer clutterAPI (`runReaderT` env) clutterServer)

clutterServer :: ServerT ClutterAPI AppM
clutterServer =
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
    :<|> serveDirectoryFileServer "static"
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
    let aidset' = Set.fromList aids'
    ---------------------------------------------------------------------------------
    -- filter with focus if any
    aidset <-
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
    let aids = V.mapMaybe (\i -> if i `Set.member` aidset then Just i else Nothing) . V.fromList $ aids'
    ---------------------------------------------------------------------------------
    envr <- envUpdateSort msb mso
    let EnvR am _ _ _ _ sn so _ _ _ = envr
    let doSort = getSort env am sn
    pure . doSort so $ aids

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
    ma <- envUpdateAlbum i
    maybe (print ("no Album" :: Text)) updateAlbumsPlayed ma
    let aj = case ma of
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

updateAlbumsPlayed :: Album -> AppM ()
updateAlbumsPlayed a = do
  --
  -- add a Note in Obsidian
  -- assumes that the Obsidian Vault is linked to `./AlbumsPlayed`
  now <- liftIO getZonedTime
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
  putTextLn "-----add entry into Day One Journal Albums Played, file in AlbumsPlayed, add to Discogs Want List"
  print js
  let iurl = fromMaybe "!!error!!" $ viaNonEmpty head js
  let itmp = "/tmp/__ac.jpg"
  exitCode <- liftIO $ rawSystem "curl" [iurl, "-o", itmp]
  unless (exitCode == ExitSuccess) $
    putTextLn $
      "ERROR getting cover img: curl failed with exit code: " <> show exitCode

  DiscogsSession ttok tu <- envGetDiscogs

  let taid = show $ albumID a
  let tinst = show $ albumInst a
  let tpls = case albumPlays a of
        6 -> "" -- ????LATB should be a configuration parameter
        _ -> show $ 1 + albumPlays a

  -- see if this release is on Want list, and get current notes
  res <-
    ( eitherDecode . TLE.encodeUtf8 . TL.pack
        <$> liftIO
          ( readProcess
              "curl"
              [ "-X"
              , "PUT"
              , "-H"
              , "Authorization: Discogs token=" <> unpack ttok
              , "-H"
              , "Content-Type: application/json; charset=utf-8"
              , "https://api.discogs.com/users/" <> unpack tu <> "/wants/" <> unpack taid
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

  -- let td = toText $ formatTime defaultTimeLocale "%Y%m%d" now
  let td = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" now
      escapeNewlines :: Text -> Text
      escapeNewlines = T.replace "\n" "\\n"

      tn = case tno of
        "" -> td
        _ -> escapeNewlines $ td <> "\n" <> tno -- need to URL-ify tno
  exitCode3 <-
    liftIO $
      rawSystem
        "curl"
        [ "-X"
        , "POST"
        , "-H"
        , "Authorization: Discogs token=" <> unpack ttok
        , "-H"
        , "Content-Type: application/json; charset=utf-8"
        , "--data"
        , "{\"notes\": \"" <> unpack tn <> "\"}"
        , "https://api.discogs.com/users/" <> unpack tu <> "/wants/" <> unpack taid
        ]
  unless (exitCode3 == ExitSuccess) $
    putTextLn $
      "ERROR adding date to Discogs Want List: failed with exit code: " <> show exitCode
  --
  -- increment played count
  let tfield = "7" -- ????LATB should be a configuration parameter
  exitCode4 <-
    liftIO $
      rawSystem
        "curl"
        [ "-X"
        , "POST"
        , "-H"
        , "Authorization: Discogs token=" <> unpack ttok
        , "-H"
        , "Content-Type: application/json; charset=utf-8"
        , "--data"
        , "{\"value\": \"" <> unpack tpls <> "\"}"
        , "https://api.discogs.com/users/" <> unpack tu <> "/collection/folders/0/releases/" <> unpack taid <> "/instances/" <> unpack tinst <> "/fields/" <> unpack tfield
        ]
  unless (exitCode4 == ExitSuccess) $
    putTextLn $
      "ERROR adding date to Discogs Want List: failed with exit code: " <> show exitCode

  putTextLn ""
  let args = ["-j", "Albums Played", "-a", itmp, "--", "new"] <> drop 1 js
  exitCode' <- liftIO $ rawSystem "dayone" args
  unless (exitCode' == ExitSuccess) $
    putTextLn $
      "ERROR adding to DayOne.app: failed with exit code: " <> show exitCode
  print args

  pure ()

-- init env from files (AppM not yet available) and run app
startApp :: Int -> Bool -> IO ()
startApp p c = envInit c >>= (run p . app)
