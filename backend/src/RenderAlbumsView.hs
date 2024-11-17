
-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}

module RenderAlbumsView ( renderAlbumsView )
where

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Lucid as L
import qualified Data.Text as T (take )
import Relude
import Text.RawString.QQ
import Types ( Env (..), EnvR (..), envGetEnvr, AppM, Album (..), SortOrder (..) )

import RenderTopMenu ( renderTopMenu )
import RenderUtil ( renderHead )

renderAlbumsView :: Text -> [Text] -> Vector Int -> AppM ( L.Html () )
renderAlbumsView ln fs aids = do
  envr <- envGetEnvr
  env <- ask
  let uhq :: Text; uhq = url env <> "albums/"
  let rbLocation :: Int -> Album -> L.Html ()
      rbLocation idx a = do
        case albumLocation a of
          Nothing ->
            case M.lookup (albumID a) (locs envr) of
              Just (loc, pos) ->
                L.div_ [L.class_ "cover-obackground2"] $ do
                  L.a_ [L.href_ (uhq <> loc <> "?sortBy=Default&sortOrder=" <> show Asc)] $
                    -- L.i_ [ L.class_ "fa fa-align-justify fa-rotate-90" ] ""
                    L.i_ [ L.class_ "fa fa-barcode" ] ""
                  L.span_ [L.class_ "loctext"] $ do
                    "Location: "
                    L.a_ [L.class_ "loclink", L.href_ (uhq <> loc <> "?sortBy=Default&sortOrder=" <> show Asc)] $
                      L.toHtml $ loc <> " #" <> show pos
              _ -> ""
          Just loc ->
            L.div_ [L.class_ "cover-obackground2"] $ do
              L.a_ [L.href_ (uhq <> loc <> "?sortBy=Default&sortOrder=" <> show Asc)] $
                L.i_ [ L.class_ "fa fa-barcode", L.style_ "color:red" ] ""
              L.span_ [L.class_ "loctext"] $ do
                "Location: "
                L.a_ [L.class_ "loclink", L.href_ (uhq <> loc <> "?sortBy=Default&sortOrder=" <> show Asc)] $
                  L.toHtml $ if loc == ln
                                then loc <> " #" <> show idx
                                else loc
  let
      albumBody :: L.Html ()
      albumBody = do
        L.div_ [L.class_ "albums"] $
        -- grid of Albums
          L.div_ [L.class_ "row"] $
            F.traverse_ renderAlbumTN . zip [1..]
                                      . mapMaybe (`M.lookup` albums envr)
                                      . V.toList $ aids

      renderAlbumTN :: (Int, Album) -> L.Html ()
      renderAlbumTN (idx, a) = do
        L.div_ [L.class_ "album-thumb"] $ do
          L.div_ [ L.class_ "cover-container"
                ] $ do
            L.div_ [L.class_ "cover-img"] $ do
              L.a_ [L.href_ ("/album/" <> show (albumID a))] $ do
                L.img_
                  [ L.src_ (albumCover a)
                  , L.class_ "cover-image"
                  , L.onerror_ "this.onerror=null;this.src='/no-cover.png';"
                  ]
            renderBadges idx a
          L.div_ [L.class_ "album-info"] $ do
            L.p_ [L.class_ "album-title"] $ L.toHtml (albumTitle a)
            L.p_ [L.class_ "album-artist"] $ L.toHtml (albumArtist a)

      renderBadges :: Int -> Album -> L.Html ()
      renderBadges idx a = do
        rbIndex idx
        rbFormat a
        rbTidal a
        rbAMusic a
        rbRating a
        rbPlays a
        rbLocation idx a

  let h = L.html_ $ do
        renderHead $ "Albums - " <> ln
        L.body_ $ do
          albumBody
          renderTopMenu env envr ln fs
          L.script_ scriptqq
  pure h

rbIndex :: Int -> L.Html ()
rbIndex idx = do
        L.div_ [L.class_ "idx"] $
          -- L.a_ [L.href_ ("http://lmini.local:8080/album/" <> show (albumID a))] $
          -- L.a_ [L.href_ (albumURL a)] $
            " " <> show idx <> " "
rbFormat :: Album -> L.Html ()
rbFormat a =
      case albumFormat a of
            "Vinyl" ->
              L.div_ [L.class_ "cover-obackground"] $ do
                L.a_ [L.href_ (albumURL a)] $ do
                  L.span_ [ L.class_ "fas fa-record-vinyl fa-sm" ] ""
              -- L.img_ [ L.src_ "/discogs-icon.png", L.alt_ "D", L.class_ "cover-oimage" ]
            "Tidal" -> L.div_ ""
            "AppleMusic" -> L.div_ [L.class_ "cover-obackground"] $ do
                L.a_ [L.href_ (albumURL a)] $ do
                  L.img_ [L.src_ "/am-icon.png", L.alt_ "A", L.class_ "cover-oimage"]
                  L.div_ ""
            "CD" ->
              L.div_ [L.class_ "cover-obackground"] $ do
                L.a_ [L.href_ (albumURL a)] $ do
                  L.span_ [ L.class_ "fas fa-compact-disc fa-sm" ] ""
            "CD, Box Set" ->
              L.div_ [L.class_ "cover-obackground"] $ do
                L.a_ [L.href_ (albumURL a)] $ do
                  L.span_ [ L.class_ "fa-stack fa-sm" ] $ do
                    -- L.span_ [L.class_ "fa fa-clone fa-stack-1x"] ""
                    L.span_ [L.class_ "fa fa-square-o fa-stack-1x"] ""
                    L.span_ [L.class_ "fa fa-compact-disc fa-stack-1x"] ""
            "Box Set, Vinyl" ->
              L.div_ [L.class_ "cover-obackground"] $ do
                L.a_ [L.href_ (albumURL a)] $ do
                  L.span_ [ L.class_ "far fa-clone fa-sm" ] ""
            "Vinyl, Box Set" ->
              L.div_ [L.class_ "cover-obackground"] $ do
                L.a_ [L.href_ (albumURL a)] $ do
                  L.span_ [ L.class_ "far fa-clone fa-sm" ] ""
            "Vinyl, Vinyl" ->
              L.div_ [L.class_ "cover-obackground"] $ do
                L.a_ [L.href_ (albumURL a)] $ do
                  L.span_ [ L.class_ "far fa-clone fa-sm" ] ""
            "Files" ->
              L.div_ [L.class_ "cover-obackground"] $ do
                L.a_ [L.href_ (albumURL a)] $ do
                  L.span_ [ L.class_ "far fa-file-audio fa-sm" ] ""
            "Streaming" ->
              L.div_ [L.class_ "cover-obackground"] $ do
                L.a_ [L.href_ (albumURL a)] $ do
                  L.span_ [ L.class_ "far fa-wifi fa-sm" ] ""
            _ ->
              L.div_ [L.class_ "cover-obackground"] $
                L.a_ [L.href_ (albumURL a)] $ do
                  L.toHtml (albumFormat a)
rbTidal :: Album -> L.Html ()
rbTidal a =
      case albumTidal a of
        Nothing -> ""
        Just tid -> L.div_ [L.class_ "cover-obackground1"] $ do
              L.a_ [L.href_ ("https://listen.tidal.com/album/" <> tid)] $ do
                L.img_ [L.src_ "/tidal-icon.png", L.alt_ "T", L.class_ "cover-oimage"]
rbAMusic :: Album -> L.Html ()
rbAMusic a =
      case albumAMusic a of
        Nothing -> ""
        Just amid -> L.div_ [L.class_ "cover-obackground3"] $ do
              if T.take 2 amid == "l."
                then L.a_ [L.href_ ("https://music.apple.com/library/albums/" <> amid)]
                else L.a_ [L.href_ ("https://music.apple.com/us/album/" <> amid)]
              $ do L.img_ [L.src_ "/am-icon.png", L.alt_ "A", L.class_ "cover-oimage"]
rbRating :: Album -> L.Html ()
rbRating a = do
        case albumRating a of
          1 -> L.div_ [L.class_ "rat"] $ do
                  L.i_ [ L.class_ "fa fa-star fa-sm" ] ""
                  L.i_ [ L.class_ "fa fa-star-o fa-sm", L.style_ "color:black" ] ""
                  L.i_ [ L.class_ "fa fa-star-o fa-sm", L.style_ "color:black" ] ""
                  L.i_ [ L.class_ "fa fa-star-o fa-sm", L.style_ "color:black" ] ""
                  L.i_ [ L.class_ "fa fa-star-o fa-sm", L.style_ "color:black" ] ""
          2 -> L.div_ [L.class_ "rat"] $ do
                  L.i_ [ L.class_ "fa fa-star fa-sm" ] ""
                  L.i_ [ L.class_ "fa fa-star fa-sm" ] ""
                  L.i_ [ L.class_ "fa fa-star-o fa-sm", L.style_ "color:black" ] ""
                  L.i_ [ L.class_ "fa fa-star-o fa-sm", L.style_ "color:black" ] ""
                  L.i_ [ L.class_ "fa fa-star-o fa-sm", L.style_ "color:black" ] ""
          3 -> L.div_ [L.class_ "rat"] $ do
                  L.i_ [ L.class_ "fa fa-star fa-sm" ] ""
                  L.i_ [ L.class_ "fa fa-star fa-sm" ] ""
                  L.i_ [ L.class_ "fa fa-star fa-sm" ] ""
                  L.i_ [ L.class_ "fa fa-star-o fa-sm", L.style_ "color:black" ] ""
                  L.i_ [ L.class_ "fa fa-star-o fa-sm", L.style_ "color:black" ] ""
          4 -> L.div_ [L.class_ "rat"] $ do
                  L.i_ [ L.class_ "fa fa-star fa-sm" ] ""
                  L.i_ [ L.class_ "fa fa-star fa-sm" ] ""
                  L.i_ [ L.class_ "fa fa-star fa-sm" ] ""
                  L.i_ [ L.class_ "fa fa-star fa-sm" ] ""
                  L.i_ [ L.class_ "fa fa-star-o fa-sm", L.style_ "color:black" ] ""
          5 -> L.div_ [L.class_ "rat"] $ do
                  L.i_ [ L.class_ "fa fa-star fa-sm" ] ""
                  L.i_ [ L.class_ "fa fa-star fa-sm" ] ""
                  L.i_ [ L.class_ "fa fa-star fa-sm" ] ""
                  L.i_ [ L.class_ "fa fa-star fa-sm" ] ""
                  L.i_ [ L.class_ "fa fa-star fa-sm" ] ""
          _ -> ""
rbPlays :: Album -> L.Html ()
rbPlays a = do
      let np = albumPlays a
      if np > 0 then
        L.div_ [L.class_ "plays"] $ do
          case np of
            cnt | cnt == 1 -> L.span_ $
                                L.i_ [ L.class_ "far fa-check-circle" ] ""
                | cnt == 2 -> L.span_ $
                                L.i_ [ L.class_ "fa fa-thermometer-1" ] ""
                | cnt == 3 -> L.span_ $
                                L.i_ [ L.class_ "fa fa-thermometer-2", L.style_ "color:orange" ] ""
                | cnt == 4 -> L.span_ $
                                L.i_ [ L.class_ "fa fa-thermometer-3", L.style_ "color:red" ] ""
                | cnt >= 5 -> L.span_ $
                                L.i_ [ L.class_ "fa fa-thermometer-4", L.style_ "color:red" ] ""
            _ -> ""
          L.span_ [L.class_ "loctext"]  ("Played " <> show (albumPlays a) <> " times")
      else ""

scriptqq :: Text
scriptqq =
  [r|
var prevScrollpos = window.pageYOffset;
window.onscroll = function() {
var currentScrollPos = window.pageYOffset;
  if (prevScrollpos > currentScrollPos) {
    document.getElementById("navbar").style.top = "0";
  } else {
    document.getElementById("navbar").style.top = "-50px";
  }
  prevScrollpos = currentScrollPos;
}
  |]

