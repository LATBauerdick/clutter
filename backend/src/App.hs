{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
  , envTidalConnect
  , envGetTag
  , envUpdateSort
  )
import qualified Lucid as L
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Text as T (stripPrefix)
import qualified Data.Vector as V (fromList, toList)
import qualified Data.IntSet as Set
import Control.Monad (foldM)
import Relude
import Render ( renderAlbumView, renderAlbumsView )
import Servant
import Types (AppM, Env (..), EnvR (..))

data HTML

newtype RawHtml = RawHtml {unRaw :: BL.ByteString}

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw


------------- Clutter API defined in Servant

type API0 = "album"
    :> Capture "aid" Int
    :> Get '[HTML] RawHtml

type API1 = "albums"
    :> Capture "list" Text
    :> QueryParam "sortBy" Text
    :> QueryParam "sortOrder" Text
    :> QueryParams "focus" Text
    :> Get '[HTML] RawHtml

type API2 = "provider"
    :> "discogs"
    :> Capture "token" Text
    :> Capture "username" Text
    :> QueryParam "nreleases" Int -- update last n releases, all if null
    :> Get '[HTML] RawHtml

type API3 = "provider"
    :> "tidal"
    :> QueryParam "nalbums" Int -- update last n tidal albums, all if null
    :> Get '[HTML] RawHtml

type ClutterAPI = API0 :<|> API1 :<|> API2 :<|> API3 :<|> Raw

clutterAPI :: Proxy ClutterAPI
clutterAPI = Proxy


-- type AppM = ReaderT Env Handler defined in Types.hs
nt :: Env -> (ReaderT Env) Handler a -> Handler a
nt env x = do
  runReaderT x env

app :: Env -> Application
app env = serve clutterAPI $ hoistServer clutterAPI (nt env) clutterServer

clutterServer :: ServerT ClutterAPI AppM
clutterServer = serveAlbum
            :<|> serveAlbums
            :<|> serveDiscogs
            :<|> serveTidal
            :<|> serveDirectoryFileServer "static"
  where
--{{{clutterServer
    serveAlbum :: Int -> AppM RawHtml
    serveAlbum aid = do
      liftIO $ print ("-------serveAlbum " :: Text, aid )
      now <- liftIO getZonedTime -- `debugId`
      ma <- envUpdateAlbum aid
      pure . RawHtml $ L.renderBS (renderAlbumView ma now)

    serveAlbums :: Text -> Maybe Text -> Maybe Text -> [Text]
                    -> AppM RawHtml
    serveAlbums listName msb mso fs = do
      env <- ask
      liftIO $ print (listName, msb, mso, fs )
    -- get ids from either list or tags (#tag)
      aids' <- case T.stripPrefix "#" listName of
        Just tag -> V.fromList <$> envGetTag tag
                      -- (fromMaybe "" (T.stripPrefix "#" tag))
        Nothing  -> getList env listName
---------------------------------------------------------------------------------------
    -- filter with focus if any
      aids'' <- if null fs then pure aids' else
                  V.fromList . Set.toList
                  <$> (
                    foldM (\ s (t, p) ->  (if p
                                            then Set.intersection s
                                            else Set.difference s
                                          ) . Set.fromList
                                          <$> envGetTag t
                      )
                      (Set.fromList . V.toList $ aids')
                    . map (\t ->  ( fromMaybe t (T.stripPrefix "-" t)
                                  , isNothing (T.stripPrefix "-" t) )
                          )
                    . mapMaybe (T.stripPrefix "#")
                    $ fs
                  )
---------------------------------------------------------------------------------------
      envr <- envUpdateSort msb mso
      let EnvR am _ _ _ sn so _ _ _ = envr
      let doSort = getSort env am sn
      let aids = doSort so aids''
      -- pure . RawHtml $
      --   L.renderBS (renderAlbumsView env envr listName fs aids)
      html <- renderAlbumsView listName fs aids
      pure . RawHtml $ L.renderBS html

    serveDiscogs :: Text -> Text -> Maybe Int -> AppM RawHtml
    serveDiscogs token username nreleases = do
      liftIO $ print ("------Updating from Discogs" :: Text, token, username, nreleases)
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
--}}}clutterServer

-- init env from files (only AppM not yet available) and run app
startApp :: IO ()
startApp = envInit >>= (run 8080 . app)

