{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Render
  ( renderAlbum,
    renderAlbums,
  )
where

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Lucid as L
import Relude
import Text.RawString.QQ
import Types (Env (..), EnvR (..), Album (..), SortOrder (..), pLocList)

renderAlbum :: Maybe Album -> L.Html ()
renderAlbum mAlbum = L.html_ $ do
  renderHead "Album Page"
  L.body_ albumBody
  where
    albumBody = 
      case mAlbum of
        Nothing ->
          L.div_ [L.class_ "login-message"] $ do
            L.p_ "Unknown Album, Sorry!"
            L.br_ []
            L.a_ [L.href_ "/albums/All"] "Please see all Albums"
        Just a ->
          case albumFormat a of
            "Tidal" ->
              L.div_ [L.class_ "login-message"] $ do
              L.p_ "Tidal Album, this view is not yet available."
              L.br_ []
              L.a_ [L.href_ "/albums/All"] "Please see all Albums"
            _ ->
              L.div_ [L.class_ "data-deskgap-drag"] $ do
              L.div_ [L.class_ "cover-container"] $ do
                L.a_ [L.href_ (albumURL a)] $ do
                  L.img_
                    [ L.src_ (albumCover a)
                    , L.alt_ "cover image"
                    , L.onerror_ "this.onerror=null;this.src='/no-cover.png';"
                    , L.class_ "cover-image"
                      ]
              L.p_ $ L.toHtml ("Title: " <> albumTitle a)
              L.p_ $ L.toHtml ("Artist: " <> albumArtist a)
              L.p_ $ L.toHtml ("Year: " <> albumReleased a)
              L.br_ []

renderAlbums :: Env -> EnvR -> Text -> Vector Int -> L.Html ()
renderAlbums env envr ln aids =
  -- L.doctype_ "html"
  L.html_ $ do
    renderHead $ "Albums - " <> ln
    L.body_ albumBody
  where
    albumBody = do
      renderLeftMenu
      -- grid of Albums
      L.div_ [L.class_ "albums"] $
        L.div_ [L.class_ "row"] $ do
          F.traverse_ renderAlbumTN $ zip [1..] (mapMaybe (`M.lookup` albums envr) (V.toList aids))

    renderLeftMenu :: L.Html ()
    renderLeftMenu =
      L.ul_ $ do

        L.li_ $ L.a_ [L.class_ "active", L.href_ (url env <> "albums/All")] "Home"

        L.li_ [L.class_ "dropdown"] $ do
          L.a_ [L.class_ "dropbtn", L.href_ "javascript:void(0)"] $do
            L.toHtml $ if pLocList ln then "List " else "List " <> ln <> " "
            L.i_ [ L.class_ "fa fa-caret-down" ] ""
          L.div_ [L.class_ "dropdown-content"] $ do
            F.traverse_ (addLink "albums/") $ V.filter (not . pLocList) (listNames envr)

        L.li_ [L.class_ "dropdown"] $ do
          L.a_ [L.class_ "dropbtn", L.href_ "javascript:void(0)"] $do
            L.toHtml $ if not (pLocList ln) then "Location " else "Location " <> ln <> " "
            L.i_ [ L.class_ "fa fa-caret-down" ] ""
          L.div_ [L.class_ "dropdown-content"] $ do
            F.traverse_ (addLink "albums/") $ V.filter pLocList (listNames envr)

        L.li_ [L.class_ "dropdown"] $ do
          let sso = case sortOrder envr of
                      Asc  -> Desc
                      Desc -> Asc
          L.a_ [L.class_ "dropbtn", L.href_ (url env <> "albums/" <> ln <> "?sortOrder=" <> show sso )] $ do
            "Sort "
            case sortName envr of
              "Default" -> ""
              _         -> L.toHtml $ "by " <> sortName envr <> " "
            case sortOrder envr of
              Asc  -> L.i_ [ L.class_ "fa fa-chevron-circle-down" ] ""
              Desc -> L.i_ [ L.class_ "fa fa-chevron-circle-up" ] ""
          L.div_ [L.class_ "dropdown-content"] $ do
            F.traverse_ (addLink ("albums/" <> ln <> "?sortBy=")) (sorts env)

      where
        addLink :: Text -> Text -> L.Html ()
        addLink t0 t1 =
          L.a_ [L.href_ (url env <> t0 <> t1)] $ do
            L.toHtml t1

    renderAlbumTN :: (Int, Album) -> L.Html ()
    renderAlbumTN (idx, a0) = do
    -- is this a Tidal entry? is there a Discog entry for this?
      let a = if albumFormat a0 /= "Tidal"
          then a0
          else fromMaybe a0
            . (`M.lookup` albums envr)
            . fromMaybe 0
            . (`M.lookup` M.fromList [(74889902, 12895083), (180866908, 18529033), (59700396, 8492595), (183187812, 18869488)])
            . fromMaybe 0 . readMaybe . toString . fromMaybe "0" . albumTidal $ a0
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
                "Tidal" -> do
                  let tid :: Int
                      tid = fromMaybe 0 . readMaybe . toString . fromMaybe "0" . albumTidal $ a
                      did :: Int
                      did = fromMaybe 0 $ M.lookup tid $ M.fromList [(74889902, 12895083), (180866908, 18529033 )]
                      mad :: Maybe Album
                      mad = M.lookup did (albums envr)
                  case mad of
                    Nothing ->  L.div_ ""
                    Just ad ->  L.div_ [L.class_ "cover-obackground"] $ do
                            L.a_ [L.href_ (albumURL ad)] $ do
                              L.span_ [ L.class_ "fas fa-compact-disc fa-sm" ] ""
                "CD" ->
                  L.div_ [L.class_ "cover-obackground"] $ do
                    L.a_ [L.href_ (albumURL a)] $ do
                      L.span_ [ L.class_ "fas fa-compact-disc fa-sm" ] ""
                "Vinyl, Box Set" ->
                  L.div_ [L.class_ "cover-obackground"] $ do
                    L.a_ [L.href_ (albumURL a)] $ do
                      L.span_ [ L.class_ "far fa-clone fa-sm" ] ""
                "File" ->
                  L.div_ [L.class_ "cover-obackground"] $ do
                    L.a_ [L.href_ (albumURL a)] $ do
                      L.span_ [ L.class_ "far fa-file-audio fa-sm" ] ""
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
                  L.a_ [L.href_ ("https://music.apple.com/us/album/" <> amid)] $ do
                    L.img_ [L.src_ "/am-icon.png", L.alt_ "A", L.class_ "cover-oimage"]
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

renderHead :: Text -> L.Html ()
renderHead t =
  L.head_ $ do
    L.title_ $ L.toHtml t
    L.meta_ [L.charset_ "utf-8"]
    L.meta_ [L.name_ "viewport", L.content_ "width=device-width, initial-scale=1.0"]
    L.meta_ [L.httpEquiv_ "X-UA-Compatible", L.content_ "ie=edge"]
    -- L.link_
    --   [ L.rel_ "stylesheet",
    --     L.type_ "text/css",
    --     L.href_ "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"
    --   ]
    let ttt :: Text; ttt = ""
    L.script_ [L.src_ "https://kit.fontawesome.com/dd23371146.js", L.crossorigin_ "anonymous"] ttt
    -- <script src="https://kit.fontawesome.com/dd23371146.js" crossorigin="anonymous"></script>
    L.style_ styleqq

styleqq :: Text
styleqq =
  [r|

@font-face {
   font-family: sansation;
   src: url(sansation_light.woff);
}

body {
   font-family: "HelveticaNeue-Light", "Helvetica Neue Light", "Helvetica Neue", Helvetica, Arial, "Lucida Grande", sans-serif; 
   font-weight: 300;
}

p {
  margin: 0 0 0 0;
}

ul {
  list-style-type: none;
  margin: 0;
  padding: 0;
  width: 12%;
  background-color: #f1f1f1;
  height: 100%; /* Full height */
  position: fixed; /* Make it stick, even on scroll */
  overflow: auto; /* Enable scrolling if the sidenav has too much content */
}

li a {
  display: block;
  color: #000;
  padding: 8px 16px;
  text-decoration: none;
}

/* Change the link color on hover */
li a:hover {
  background-color: #555;
  color: white;
}

.active {
  background-color: #333;
  color: white;
}

.dropbtn {
  background-color: #ff7000 /* #4CAF50*/;
  color: white;
  padding: 16px;
  font-size: 16px;
  border: none;
  cursor: pointer;
}

.dropdown {
  position: relative;
  display: inline-block;
}

.dropdown-content {
  display: none;
  position: absolute;
  background-color: #f9f9f9;
  min-width: 160px;
  box-shadow: 0px 8px 16px 0px rgba(0,0,0,0.2);
  z-index: 1;
}

.dropdown-content a {
  color: black;
  padding: 12px 16px;
  text-decoration: none;
  display: block;
}

.dropdown-content a:hover {background-color: #f9c87c /* #f1f1f1 */}

.dropdown:hover .dropdown-content {
  display: block;
}

.dropdown:hover .dropbtn {
  background-color: #f97432 /* #3e8e41 */;
}


* {
  box-sizing: border-box;
}

img {
/*  border: 1px solid #ddd; */
  border-radius: 4px;
  padding: 5px;
}

img:hover {
  box-shadow: 0 0 2px 1px rgba(0, 140, 186, 0.5);
}

.album-thumb {
  padding: 0px 1px 10px;
}

.album-info {
  width: 210px;
}

p.album-title {
  white-space: nowrap; 
  /* border: 1px solid #ddd; */
  overflow: hidden;
  text-overflow: ellipsis;
  font-family: helvetica;
  font-size: 14px;
  margin: 2px 0 0 0;
}
p.album-artist {
  white-space: nowrap; 
  /* border: 1px solid #ddd; */
  overflow: hidden;
  text-overflow: ellipsis;
  font-family: helvetica;
  font-size: 11px;
  margin: 4px 0 0 0;
}

.albums {
  margin-left:12%;
  padding:1px 16px;
/*  height:1000px; */
 }

.row {
  display: flex;
  justify-content: space-evenly;
  flex-wrap: wrap;
  padding: 0 1px;
}

/* Container needed to position the overlay. Adjust the width as needed */
.cover-container {
  width:  205px;
  height: 205px;
  position: relative;
  text-align: center;
  vertical-align: middle;
}
.cover-container:hover {
  box-shadow: 0 0 2px 1px rgba(0, 140, 186, 0.5);
}

/* Make the image to responsive */
.cover-image {
  display: block;
  width: 100%;
  height: 100%;
  border-radius: 4px;
  padding: 5px;
  position: absolute;
  top: 0;
  left: 0;
}
.cover-image:hover {
  box-shadow: 0 0 2px 1px rgba(0, 140, 186, 0.5);
}

.cover-obackground {
  width: 16px;
  height: 16px;
  padding-top: 1px;
  border-radius: 4px;
  position: absolute;
  right: 7;
  bottom: 7;
  background-color: rgba(255,255,255,.5);
}
.cover-obackground a:link { color: black; }
.cover-obackground a:visited { color: black; }
.cover-obackground a:hover { color: red; }

.cover-oimage {
  display: block;
  width: 16px;
  height: 16px;
  padding: 0px;
}
a:link {
  color: black;
}
a:visited {
  color: black;
}
a:hover {
  color: rgba(0, 140, 186, 0.5);
}
a:active {
  color: blue;
}

.rat {
  width: 70px;
  height: 14px;
  font-size:small;
  color: gold;
  padding-top: 1px;
  border-radius: 4px;
  position: absolute;
  left: 87;
  bottom: 7;
  background-color: rgba(255,255,255,.5);
}
.rat0 {
  width: 70px;
  height: 13px;
  font-size:small;
  color: black;
  padding: 0px;
  border-radius: 4px;
  position: absolute;
  left: 87;
  bottom: 7;
}
.plays {
  width: 15px;
  height: 14px;
  font-size:small;
  color: black;
  padding-top: 1px;
  border-radius: 4px;
  position: absolute;
  left: 71;
  bottom: 7;
  background-color: rgba(255,255,255,.5);
}
.plays .loctext {
  visibility: hidden;
  width: 120px;
  padding-top: 1px;
  background-color: black;
  color: #777;
  padding: 5px 0;
  border-radius: 6px;
  position: absolute;
  z-index: 1;
}
.plays:hover .loctext {
  visibility: visible;
}

.idx {
  height: 18px;
  padding-left: 4px;
  padding-right: 4px;
  text-align: left;
  position: absolute;
  top: 1;
  left: 1;
  display: inline-block;
  border-radius: 9px;
  background-color: rgba(155,155,155,.5);
}

.loc {
  height: 16px;
  position: relative;
  display: inline-block;
  border-radius: 8px;
  background-color: rgba(155,155,155,.5);
}

.loc .loctext {
  visibility: hidden;
  width: 120px;
  padding-top: 1px;
  background-color: black;
  color: #777;
  padding: 5px 0;
  border-radius: 6px;
  position: absolute;
  z-index: 1;
}

.loc:hover .loctext {
  visibility: visible;
}
.loclink {
  color: #0ff;
}


.cover-obackground1 {
  width: 16px;
  height: 16px;
  border-radius: 4px;
  border-radius: 4px;
  position: absolute;
  left: 7;
  bottom: 7;
  background-color: rgba(255,255,255,.5);
}

.cover-obackground3 {
  width: 16px;
  height: 16px;
  border-radius: 4px;
  border-radius: 4px;
  position: absolute;
  left: 27;
  bottom: 7;
  background-color: rgba(255,255,255,.5);
}

.cover-obackground2 {
  height: 16px;
  width: 20px;
  color: black;
  border-radius: 4px;
  position: absolute;
  left: 49;
  bottom: 7;
  background-color: rgba(255,255,255,.5);
}

.cover-obackground2 a:link { color: black; }
.cover-obackground2 a:visited { color: black; }
.loctext a:link { color: white; }
.loctext a:visited { color: white; }

.cover-obackground2 .loctext {
  visibility: hidden;
  width: 120px;
  background-color: black;
  color: #fff;
  padding: 5px 0;
  border-radius: 6px;
  position: absolute;
  z-index: 1;
}

.cover-obackground2:hover .loctext {
  visibility: visible;
}

|]
