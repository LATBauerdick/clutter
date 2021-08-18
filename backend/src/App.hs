{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App
  ( startApp,
    app,
  )
where

import qualified Data.ByteString.Lazy as BL
import Data.Time (getZonedTime)

import Env
  ( envInit
  , envUpdate
  , envUpdateAlbum
  , envGetEnvr
  , envGetTag
  , envUpdateSort
  )
import qualified Lucid as L
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Text as T (stripPrefix)
import Data.Vector as V (fromList, toList)
import Data.List as L (intersect)
import Relude
import Render ( renderAlbumView, renderAlbumsView )
import Servant
import Types (Env (..), EnvR (..))

data HTML = HTML

newtype RawHtml = RawHtml {unRaw :: BL.ByteString}

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

------------------ Servant Stuff

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
    :> Capture "token" Text
    :> Capture "username" Text
    :> QueryParam "nreleases" Int -- update last n releases, all if null
    :> Get '[HTML] RawHtml

type API = API0 :<|> API1 :<|> API2 :<|> Raw

api :: Proxy API
api = Proxy

server :: Env -> Server API
server env = serveAlbum
          :<|> serveAlbums
          :<|> serveDiscogs
          :<|> serveDirectoryFileServer "static"
  where
    serveAlbum :: Int -> Handler RawHtml
    serveAlbum aid = do
      now <- liftIO getZonedTime -- `debugId`
      ma <- liftIO $ envUpdateAlbum env aid
      pure . RawHtml $ L.renderBS (renderAlbumView ma now)

    serveAlbums :: Text -> Maybe Text -> Maybe Text -> [Text]
                    -> Handler RawHtml
    serveAlbums listName msb mso fs = do
      liftIO $ print (listName, msb, mso, fs )
    -- get ids from either list or tags (#tag)
      aids' <- case T.stripPrefix "#" listName of
        Just tag -> liftIO $ V.fromList <$> envGetTag env tag
                      -- (fromMaybe "" (T.stripPrefix "#" tag))
        Nothing  -> liftIO $ getList env env listName
    -- filter with focus if any
      aids'' <- case viaNonEmpty head fs of
        Just t -> V.fromList
                  . L.intersect (V.toList aids')
                    <$> liftIO
                      ( envGetTag env (fromMaybe "" (T.stripPrefix "#" t)))
        Nothing -> pure aids'
      envr <- liftIO $ envUpdateSort env msb mso
      let EnvR am _ _ _ sn so _ _ _ = envr
      let doSort = getSort env am sn
      let aids = doSort so aids''
      pure . RawHtml $
        L.renderBS (renderAlbumsView env envr listName fs aids)

    serveDiscogs :: Text -> Text -> Maybe Int -> Handler RawHtml
    serveDiscogs token username nreleases = do
      liftIO $ print (token, username, nreleases)
      _ <- liftIO (envUpdate env token username (fromMaybe 0 nreleases))
      envr <- liftIO $ envGetEnvr env
      let ln = "Discogs"
      aids <- liftIO (getList env env ln)
      pure . RawHtml $ L.renderBS (renderAlbumsView env envr ln [] aids)

startApp :: IO ()
startApp = envInit >>= (run 8080 . app)

app :: Env -> Application
app env = serve api $ server env
