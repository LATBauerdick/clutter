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
  , envUpdateSort
  )
import qualified Lucid as L
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Relude
import Render
  ( renderAlbum,
    renderAlbums,
  )
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
    :> Get '[HTML] RawHtml

type API2 =
  "add"
    :> Capture "releaseid" Int
    :> Get '[HTML] RawHtml

type API3 =
  "provider"
    :> "discogs"
    :> Capture "token" Text
    :> Capture "username" Text
    :> Get '[HTML] RawHtml

type API4 =
  "provider"
    :> "tidal"
    :> Capture "token" Text
    :> Capture "username" Text
    :> Get '[HTML] RawHtml

type API = API0 :<|> API1 :<|> API2 :<|> API3 :<|> API4 :<|> Raw

api :: Proxy API
api = Proxy

server :: Env -> Server API
server env =
  serveAlbum
    :<|> serveAlbums
    :<|> serveAdd
    :<|> serveDiscogs
    :<|> serveTidal
    :<|> serveDirectoryFileServer "static"
  where
    serveAlbum :: Int -> Handler RawHtml
    serveAlbum aid = do
      now <- liftIO getZonedTime -- `debugId`
      ma <- liftIO $ envUpdateAlbum env aid
      pure . RawHtml $ L.renderBS (renderAlbum ma now)

    serveAlbums :: Text -> Maybe Text -> Maybe Text -> Handler RawHtml
    serveAlbums listName msb mso = do
      aids' <- liftIO (getList env env listName)
      envr <- liftIO $ envUpdateSort env msb mso
      let EnvR am _ _ _ sn so _ = envr
      let doSort = getSort env am sn
      let aids = doSort so aids'
      pure . RawHtml $
        L.renderBS (renderAlbums env envr listName aids)
    -- serveJSON :: Server API2
    -- serveJSON = do
    --   pure $ M.elems ( albums env )

    serveAdd :: Int -> Handler RawHtml
    serveAdd aid = do
      now <- liftIO getZonedTime -- `debugId`
      ma <- liftIO $ envUpdateAlbum env aid
      pure . RawHtml $ L.renderBS (renderAlbum ma now)
      -- fromEnum TDiscogs

    serveDiscogs :: Text -> Text -> Handler RawHtml
    serveDiscogs token username = do
      _ <- liftIO (envUpdate env token username)
      envr <- liftIO $ envGetEnvr env
      let ln = "Discogs"
      aids <- liftIO (getList env env ln)
      pure . RawHtml $ L.renderBS (renderAlbums env envr ln aids)

    serveTidal :: Text -> Text -> Handler RawHtml
    serveTidal token username = do
      _ <- liftIO (envUpdate env token username)
      envr <- liftIO $ envGetEnvr env
      let ln = "Tidal"
      aids <- liftIO (getList env env ln)
      pure . RawHtml $ L.renderBS (renderAlbums env envr ln aids)

startApp :: IO ()
startApp = envInit >>= (run 8080 . app)

app :: Env -> Application
app env = serve api $ server env
