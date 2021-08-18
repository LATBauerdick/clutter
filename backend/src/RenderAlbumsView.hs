
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RenderAlbumsView ( renderAlbumsView )
where

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Lucid as L
import qualified Data.Text as T (take, stripPrefix, find)
import Relude
import Text.RawString.QQ
import Types (Env (..), EnvR (..), Album (..), SortOrder (..), pLocList)

import RenderUtil ( renderHead )

renderAlbumsView :: Env -> EnvR -> Text -> [Text] -> Vector Int -> L.Html ()
renderAlbumsView env envr ln fs aids =
  -- L.doctype_ "html"
  L.html_ $ do
    renderHead $ "Albums - " <> ln
    L.body_ $ do
      albumBody
      L.script_ scriptqq
  where
    lnq = maybe ln ( "%23" <> ) (T.stripPrefix "#" ln) <> "?"
    lnqq = lnq <> maybe "" ("&focus=%23" <>)
                          (T.stripPrefix "#" =<< viaNonEmpty head fs)
    albumBody :: L.Html ()
    albumBody = do
      let topMenu = True
      if topMenu then "" else renderTopMenu
      L.div_ [L.class_ (if topMenu
                          then "albums"
                          else "albums12perc"
                        )] $
      -- grid of Albums
        L.div_ [L.class_ "row"] $ do
          F.traverse_ renderAlbumTN
            $ zip [1..]
                  (mapMaybe
                    (`M.lookup` albums envr)
                    (V.toList aids)
                  )
      if topMenu then renderTopMenu else ""

    renderTopMenu :: L.Html ()
    renderTopMenu =
      L.div_ [L.id_ "navbar"] $ do
        L.div_ [L.class_ "dropdown"] renderFocus
        L.a_  [L.class_ "active"
              , L.href_ (url env <> "albums/2021%20Listened?sortOrder=Desc")] "Listened"
        L.a_  [L.href_ (url env <> "albums/Discogs")] "Discogs"
        L.div_ [L.class_ "dropdown"] renderButtonList
        L.div_ [L.class_ "dropdown"] renderButtonLocation
        L.div_ [L.class_ "dropdown"] renderButtonTags
        L.div_ [L.class_ "dropdown"] renderButtonSort
        L.div_ [L.class_ "dropdown"] renderButtonOrder

    addLink :: Text -> Text -> L.Html ()
    addLink t0 t1 =
      L.a_ [L.href_ (url env <> t0 <> t1)] $ do
        L.toHtml t1


    renderButtonList = do
      L.button_ [L.class_ "dropbtn"] $do
        L.toHtml $ if pLocList ln || isJust (T.stripPrefix "#" ln) then "List " else  "List " <> ln <> " "
        L.i_ [ L.class_ "fa fa-caret-down" ] ""
      L.div_ [L.class_ "dropdown-content"] $ do
        F.traverse_ (addLink "albums/") . filter (not . pLocList) $ M.keys (listNames envr)

    renderButtonLocation = do
      L.button_ [L.class_ "dropbtn"] $do
        L.toHtml $ if not (pLocList ln) then "Location " else "Location " <> ln <> " "
        L.i_ [ L.class_ "fa fa-caret-down" ] ""
      L.div_ [L.class_ "dropdown-content"] $ do
        F.traverse_ (addLink "albums/") . filter pLocList $ M.keys (listNames envr)

    renderButtonTags = do
      L.button_ [L.class_ "dropbtn"] $do
        L.toHtml $ if isJust (T.stripPrefix "#" ln)
                    then "Tags " <> ln <> " "
                    else "Tags"
        L.i_ [ L.class_ "fa fa-caret-down" ] ""
      L.div_ [L.class_ "dropdown-content"] $ do
        F.traverse_ (addLink "albums/%23") $ M.keys (tags envr)

    renderButtonSort = do
      L.button_ [L.class_ "dropbtn"] $ do
        L.toHtml $ "Sort " <> if sortName envr /= "Default"
                    then "by " <> sortName envr <> " "
                    else ""
      L.div_ [L.class_ "dropdown-content"] $ do
        F.traverse_ (addLink ("albums/" <> lnqq <> "&sortBy=")) (sorts env)

    renderButtonOrder = do
      let sso = case sortOrder envr of
                  Asc  -> Desc
                  Desc -> Asc
      L.a_  [L.href_ (url env <> "albums/"
                        <> lnqq <> "&sortOrder=" <> show sso
                        )] $
          case sortOrder envr of
            Asc ->  L.i_ [ L.class_ "fa fa-chevron-circle-down" ] ""
            Desc -> L.i_ [ L.class_ "fa fa-chevron-circle-up" ] ""


    -- renderButtonFocus = do
    --   L.button_ [L.class_ "dropbtn"] $do
    --     L.toHtml $ if  fs /= []
    --                  then "Focus " <> fromMaybe "" (viaNonEmpty head fs)
    --                  else "Focus "
    --     L.i_ [ L.class_ "fa fa-caret-down" ] ""
    --   L.div_ [L.class_ "dropdown-content"] $ do
    --     F.traverse_ (addLink ("albums/" <> lnq <> "&focus=%23"))
    --                 $ filter (isJust . T.find ('.' ==)) (M.keys (tags envr)) <> filter (isNothing . T.find ('.' ==)) (M.keys (tags envr))

    renderFocus = do
      L.button_ [L.class_ "dropbtn"] $do
        L.toHtml $ if  fs /= []
                     then "Focus " <> fromMaybe "" (viaNonEmpty head fs)
                     else "Focus "
        L.i_ [ L.class_ "fa fa-caret-down" ] ""
      L.div_ [L.class_ "focus-content"] $ do
        F.traverse_ (addLink ("albums/" <> lnq <> "&focus=%23"))
                    $ filter (isJust . T.find ('.' ==)) (M.keys (tags envr)) <> filter (isNothing . T.find ('.' ==)) (M.keys (tags envr))

    renderLeftMenu :: L.Html ()
    renderLeftMenu =
      L.ul_ $ do
        L.li_ [L.class_ "dropdown"] renderButtonList
        L.li_ [L.class_ "dropdown"] renderButtonLocation
        L.li_ [L.class_ "dropdown"] renderButtonTags
        L.li_ [L.class_ "dropdown"] renderButtonSort
        L.li_ [L.class_ "dropdown"] renderButtonOrder
        L.li_ [L.class_ "dropdown"] renderFocus

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
          case albumFormat a of
                "Vinyl" ->
                  L.div_ [L.class_ "cover-obackground"] $ do
                    L.a_ [L.href_ (albumURL a)] $ do
                      L.span_ [ L.class_ "fas fa-record-vinyl fa-sm" ] ""
                  -- L.img_ [ L.src_ "/discogs-icon.png", L.alt_ "D", L.class_ "cover-oimage" ]
                "Tidal" -> L.div_ ""
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
                "File" ->
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
          case albumTidal a of
            Nothing -> ""
            Just tid -> L.div_ [L.class_ "cover-obackground1"] $ do
                  L.a_ [L.href_ ("https://listen.tidal.com/album/" <> tid)] $ do
                    L.img_ [L.src_ "/tidal-icon.png", L.alt_ "T", L.class_ "cover-oimage"]
          case albumAM a of
            Nothing -> ""
            Just amid -> L.div_ [L.class_ "cover-obackground3"] $ do
                  if T.take 2 amid == "l."
                    then L.a_ [L.href_ ("https://music.apple.com/library/albums/" <> amid)]
                    else L.a_ [L.href_ ("https://music.apple.com/us/album/" <> amid)]
                  $ do L.img_ [L.src_ "/am-icon.png", L.alt_ "A", L.class_ "cover-oimage"]
          let showLocation = True
          if showLocation then
            case albumLocation a of
              Nothing ->
                case M.lookup (albumID a) (locs envr) of
                  Just (loc, pos) ->
                    L.div_ [L.class_ "cover-obackground2"] $ do
                      L.a_ [L.href_ (url env <> "albums/" <> loc <> "?sortBy=Default&sortOrder=" <> show Asc)] $
                        -- L.i_ [ L.class_ "fa fa-align-justify fa-rotate-90" ] ""
                        L.i_ [ L.class_ "fa fa-barcode" ] ""
                      L.span_ [L.class_ "loctext"] $ do
                        "Location: "
                        L.a_ [L.class_ "loclink", L.href_ (url env <> "albums/" <> loc <> "?sortBy=Default&sortOrder=" <> show Asc)] $
                          L.toHtml $ loc <> " #" <> show pos
                  _ -> ""
              Just loc ->
                L.div_ [L.class_ "cover-obackground2"] $ do
                  L.a_ [L.href_ (url env <> "albums/" <> loc <> "?sortBy=Default&sortOrder=" <> show Asc)] $
                    L.i_ [ L.class_ "fa fa-barcode", L.style_ "color:red" ] ""
                  L.span_ [L.class_ "loctext"] $ do
                    "Location: "
                    L.a_ [L.class_ "loclink", L.href_ (url env <> "albums/" <> loc <> "?sortBy=Default&sortOrder=" <> show Asc)] $
                      L.toHtml $ if loc == ln
                                    then loc <> " #" <> show idx
                                    else loc
            else ""
          let showNumbers = True
          if showNumbers then
            L.div_ [L.class_ "idx"] $
              -- L.a_ [L.href_ ("http://lmini.local:8080/album/" <> show (albumID a))] $
              -- L.a_ [L.href_ (albumURL a)] $
                " " <> show idx <> " "
            else ""
          let showRating = True
          if showRating then
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
            else ""
          let showPlays = True
          let np = albumPlays a
          if showPlays && np > 0 then
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
        L.div_ [L.class_ "album-info"] $ do
          L.p_ [L.class_ "album-title"] $ L.toHtml (albumTitle a)
          L.p_ [L.class_ "album-artist"] $ L.toHtml (albumArtist a)


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

