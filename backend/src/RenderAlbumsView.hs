
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
import qualified Data.Text as T (take, stripPrefix, find, intercalate )
import Relude
import Text.RawString.QQ
import Types (Env (..), EnvR (..), envGetEnvr, AppM, Album (..), SortOrder (..), pLocList)

import RenderUtil ( renderHead, formUrlEncodeQuery )

renderAlbumsView :: Text -> [Text] -> Vector Int -> AppM ( L.Html () )
renderAlbumsView ln fs aids = do
  envr <- envGetEnvr
  env <- ask
  let ffs :: [Text]
      ffs = mapMaybe (T.stripPrefix "#") fs
  let fqs :: Map Text Bool
      fqs = M.fromList
                . map (\t ->  (fromMaybe t (T.stripPrefix "-" t)
                              , isNothing (T.stripPrefix "-" t)))
                $ ffs
  let uhq :: Text; uhq = url env <> "albums/"
      qry' :: [Text] -> Text -> Text -- create the query url
      qry' ts n = uhq
                <> n
                <> "?"
                <> ( decodeUtf8 . formUrlEncodeQuery
                    . map (\t -> ("focus", toString t))
                    $ ts )
  let sts :: [Text] -- sorted tags
      sts = filter (isJust . T.find ('.' ==)) (M.keys (tags envr))
          <> filter (isNothing . T.find ('.' ==)) (M.keys (tags envr))

  let
      albumBody :: L.Html ()
      albumBody = do
        L.div_ [L.class_ "albums"] $
        -- grid of Albums
          L.div_ [L.class_ "row"] $
            F.traverse_ renderAlbumTN . zip [1..]
                                      . mapMaybe (`M.lookup` albums envr)
                                      . V.toList $ aids
        renderTopMenu

      renderTopMenu :: L.Html ()
      renderTopMenu =
        L.div_ [L.id_ "navbar"] $ do
          L.div_ [L.class_ "dropdown"] renderShow
          L.div_ [L.class_ "dropdown"] renderFocus
          L.div_ [L.class_ "dropdown"] renderButtonSort
          L.div_ [L.class_ "dropdown"] renderButtonOrder
          L.a_   [L.class_ "active"
                 , L.href_ (uhq <> "2022 Listened?&sortBy=Default&sortOrder=Desc")] "Listened"
          L.a_   [L.class_ "active", L.href_ (uhq <> "Discogs")] "Discogs"
          L.div_ [L.class_ "dropdown"] renderButtonList
          L.div_ [L.class_ "dropdown"] renderButtonLocation
          L.div_ [L.class_ "dropdown"] renderButtonTags

      addLink :: Text -> Text -> L.Html ()
      addLink t0 t1 =
        L.a_ [L.href_ (t0 <> t1)] $ do
          L.toHtml t1

      renderShow = do
        L.button_ [L.class_ "dropbtn"] $do
          L.a_  [L.class_ "dropbtn", L.href_ (qry' fs ln)] $ L.toHtml $ "Showing " <> ln

      renderButtonList = do
        L.button_ [L.class_ "dropbtn"] $ do
          "List "
          L.i_ [ L.class_ "fa fa-caret-down" ] ""
        L.div_ [L.class_ "dropdown-content"] $ do
          F.traverse_ (\x -> L.a_ [L.href_ (qry' fs x)] $ do L.toHtml x)
            . filter (not . pLocList) $ M.keys (listNames envr)

      renderButtonLocation = do
        L.button_ [L.class_ "dropbtn"] $ do
          "Location "
          L.i_ [ L.class_ "fa fa-caret-down" ] ""
        L.div_ [L.class_ "dropdown-content"] $ do
          F.traverse_ (\x -> L.a_ [L.href_ (qry' fs x)] $ do L.toHtml x)
            . filter pLocList $ M.keys (listNames envr)

      renderButtonTags = do
        L.button_ [L.class_ "dropbtn"] $do
          "Tags "
          L.i_ [ L.class_ "fa fa-caret-down" ] ""
        L.div_ [L.class_ "dropdown-content"] $ do
          F.traverse_ (\x -> L.a_ [L.href_ (qry' fs ("#" <> x))] $ do L.toHtml x)
            sts -- M.keys (tags envr)

      renderButtonSort = do
        L.button_ [L.class_ "dropbtn"] $ do
          L.toHtml $ "Sort " <> if sortName envr /= "Default"
                      then "by " <> sortName envr <> " "
                      else ""
        L.div_ [L.class_ "dropdown-content"] $ do
          F.traverse_ (addLink ((qry' fs ln) <> "&sortBy=")) (sorts env)

      renderButtonOrder = do
        L.button_ [L.class_ "dropbtn-order"] $ do
          let sso = case sortOrder envr of
                      Asc  -> Desc
                      Desc -> Asc
          L.a_  [L.href_ ((qry' fs ln) <> "&sortOrder=" <> show sso)] $
              case sortOrder envr of
                Asc ->  L.i_ [ L.class_ "fa fa-chevron-circle-down" ] ""
                Desc -> L.i_ [ L.class_ "fa fa-chevron-circle-up" ] ""

      renderFocus = do
        let lnk :: Map Text Bool -> Text -> L.Html ()
            lnk ts t = do
              let nts = map (\(it, ip) -> if ip then "#" <> it else "#-" <> it)
                        . M.toList
                        . ttt $ ts
                        -- . M.insertWith xor t True $ ts
                  p = isJust $ t `M.lookup` ts -- this tag was selected
                  pp = fromMaybe False $ t `M.lookup` ts -- tag was True
                  ttt:: Map Text Bool -> Map Text Bool; ttt tts
                    | p && pp   = M.insert t False tts
                    | p         = M.delete t tts
                    | otherwise = M.insert t True tts
                  tc :: Text; tc
                    | p && pp   = "focus-on"
                    | p         = "focus-not"
                    | otherwise = "focus"
              L.a_  [ L.class_ tc
                    , L.href_ (qry' nts ln)] $ do
                L.toHtml t

        L.button_ [L.class_ "dropbtn"] $do
          L.a_  [L.class_ "dropbtn", L.href_ (qry' [] ln) ] $ do --L.toHtml $ "Showing " <> ln
            L.toHtml $ if  ffs /= []
                        then "Focus (#" <> T.intercalate " #" ffs <> ")"
                        else "Focus "
            L.i_ [ L.class_ "fa fa-caret-down" ] ""

        L.div_ [L.class_ "focus-content"] $ do
          F.traverse_ (lnk fqs) sts

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

  let h = L.html_ $ do
        renderHead $ "Albums - " <> ln
        L.body_ $ do
          albumBody
          L.script_ scriptqq
  pure h

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

