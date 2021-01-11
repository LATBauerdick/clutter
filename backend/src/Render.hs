{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}



module Render
    ( renderAlbum
    , renderAlbums
    ) where

-- import Network.Wai
-- import Network.Wai.Handler.Warp
-- import Servant

import qualified Data.Map.Strict as M
import qualified Data.Foldable as F
import Data.Maybe ( mapMaybe )
import qualified Data.Vector as V ( toList, fromList )

-- import qualified Data.ByteString.Lazy as BL
import qualified Lucid as L
-- import Network.HTTP.Media ((//), (/:))

import Data.Text ( Text )

import qualified Data.Text as T
import Text.RawString.QQ

import Env ( Env (..) )
import Provider ( Album (..) )

renderAlbum :: Maybe Album -> L.Html ()
renderAlbum mAlbum = L.html_ $ do
  renderHead "Album Page"
  L.body_ albumBody
  where
    albumBody = case mAlbum of
      Nothing -> L.div_ [L.class_ "login-message"] $ do
          L.p_ "Unknown Album, Sorry!"
          L.br_ []
          L.a_ [L.href_ "/albums"] "Please see all Albums"
      Just a -> L.div_ [L.class_ "data-deskgap-drag"] $ do
          L.div_ [L.class_ "cover-art"] $ do
            L.img_ [ L.src_ (albumCover a), L.width_ "160"
                   , L.height_ "160", L.alt_ "cover image." ]
          L.p_ $ L.toHtml ("Title: " ++ T.unpack ( albumTitle a ))
          L.p_ $ L.toHtml ("Artist: " ++ T.unpack ( albumArtist a ))
          L.p_ $ L.toHtml ("Year: " ++ T.unpack (albumReleased a))
          L.br_ []

renderAlbums :: Env -> Text -> Text -> L.Html ()
renderAlbums env ln sn =
    -- L.doctype_ "html"
    L.html_ $ do
      renderHead $ "Albums - " <> ln
      L.body_ albumBody
      where
        addLink :: Text -> L.Html ()
        addLink t =
              L.a_ [ L.href_ ( url env <>  "albums/" <> t ) ] $ do
                L.toHtml ( T.unpack t )

        albumBody = do
-- Left Menu
          L.ul_ $ do
            L.li_ $ L.a_ [ L.class_ "active", L.href_ ( url env <> "albums/All" ) ] "Home"
            L.li_ [L.class_ "dropdown"] $ do
              L.a_ [ L.class_ "dropbtn", L.href_ "javascript:void(0)" ] 
                ( L.toHtml ( "List " ++ T.unpack ln ))
              L.div_ [ L.class_ "dropdown-content" ] $ do
                F.traverse_ addLink  ( lists env )

            L.li_ [L.class_ "dropdown"] $ do
              L.a_ [ L.class_ "dropbtn", L.href_ "javascript:void(0)" ] 
                ( L.toHtml ( "Sort by " ++ T.unpack sn ))
              L.div_ [ L.class_ "dropdown-content" ] $ do
                F.traverse_ addLink  ( sorts env )

            L.li_ $ L.a_ [ L.href_ ( url env ) ] "index"
-- grid of Albums
          L.div_ [L.class_ "albums"] $ do
            L.div_ [L.class_ "row"] $ do
              renderTNs env ln sn

renderTNs :: Env -> Text -> Text -> L.Html ()
renderTNs env ln sn =
  F.traverse_ renderAlbumTN albumList where
    sort = getSort env sn
    aids = getList env ln
    albumList = V.fromList $
        mapMaybe ( `M.lookup` albums env )
        ( V.toList (sort aids) )

renderAlbumTN :: Album -> L.Html ()
renderAlbumTN a =
        L.div_ [L.class_ "album-thumb"] $ do
          L.div_ [L.class_ "cover-art"] $ do
            -- L.a_ [L.href_ $ T.pack ("https://www.discogs.com/release/" ++ show (albumID a))] $ do
            -- L.a_ [L.href_ $ T.pack ("https://www.tidal.com/album/" ++ show (albumID a))] $ do
            L.a_ [L.href_ (albumURL a a)] $ do
              L.img_ [ L.src_ (albumCover a), L.alt_ "cover image." ]
          L.div_ [L.class_ "album-info"] $ do
            L.p_ [L.class_ "album-title"] $ do
              L.toHtml (T.unpack ( albumTitle a ))
            L.p_ [L.class_ "album-artist"] $ do
              L.toHtml (T.unpack ( albumArtist a ))


renderHead :: Text -> L.Html ()
renderHead t =
      L.head_ $ do
        L.title_ $ L.toHtml ( T.unpack t )
        L.meta_ [L.charset_ "utf-8"]
        L.meta_ [L.name_ "viewport", L.content_ "width=device-width, initial-scale=1.0"]
        L.meta_ [L.httpEquiv_ "X-UA-Compatible", L.content_ "ie=edge"]
        L.link_ [L.rel_ "stylesheet", L.type_ "text/css", L.href_ "/styles.css"]
        L.style_ styleqq

styleqq :: Text
styleqq = [r|
* {
  box-sizing: border-box;
}

|]

