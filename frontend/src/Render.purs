module Render (
  render
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (fromRight)
import Data.Tuple (Tuple(..))
import Data.Array (range, length, zip)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
-- import Halogen.HTML.CSS as CSS
-- import CSS (backgroundColor, fontSize, px)

import Data.DateTime (DateTime)
import Data.Formatter.DateTime (formatDateTime)
import Data.String (take) as S
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))

import Types (Album, State, AlbumList(..), Action(..), SortOrder(..))
import RenderTopMenu (renderTopMenu)

render :: forall m. State -> H.ComponentHTML Action () m
render state = do
  HH.div_
    [ case state.listName of
          AlbumList Nothing  -> albumView state.album state.now
          _                  -> renderListView state
    , renderTopMenu state
    , HH.div_
      [ HH.br_, HH.br_, HH.br_
      , HH.form
        [ HE.onSubmit \ev -> MakeRequest ev ]
        [ HH.h3_ [ HH.text "Look up Album ID" ]
        , HH.label_
            [ HH.div_ [ HH.text "Enter album id:" ]
            , HH.input
                [ HP.value state.albumID
                , HE.onValueInput \str -> SetAlbumID str
                ]
            ]
        , HH.button
            [ HP.disabled state.loading
            , HP.type_ HP.ButtonSubmit
            ]
            [ HH.text "Fetch info" ]
        , HH.h1_
            [ HH.text if state.loading then "Working..." else "" ]
        ]
    -- , HH.div_
    --     case state.result of
    --       Nothing -> []
    --       Just res ->
    --         [ HH.h2_
    --             [ HH.text "Response:" ]
    --         , HH.pre_
    --             [ HH.code_ [ HH.text res ] ]
    --         ]
    -- , HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
    -- , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
      ]
    ]


renderAlbumTN :: forall m. Int -> Album -> H.ComponentHTML Action () m
renderAlbumTN idx a =
  HH.div
    [ HP.class_ $ HH.ClassName "album-thumb" ]
    [ HH.div
      [ HP.class_ $ HH.ClassName "cover-container" ]
      [ HH.div
        [ HP.class_ $ HH.ClassName "cover-img" ] $
        [ HH.button
          [ HE.onClick \_ -> ShowAlbum (show a.albumID) ]
          [ HH.img [ HP.src a.albumCover
                   , HP.alt "cover image"
                   --, HP.onerror "this.onerror=null;this.src='http://localhost:8080/no-cover.png';"
                   , HP.class_ $ HH.ClassName "cover-image"
                   ]
          ]
        ]
        <> renderBadges idx a
      ]
    , HH.div [ HP.class_ $ HH.ClassName "album-info" ]
        [ HH.p
          [ HP.class_ $ HH.ClassName "album-title"]
          [ HH.text a.albumTitle ]
        , HH.p
          [ HP.class_ $ HH.ClassName "album-artist"]
          [ HH.text a.albumArtist ]
        ]
    ]

renderListView :: forall m. State -> H.ComponentHTML Action () m
renderListView state =
  let as = state.albumList
  in
  HH.div
  [ HP.class_ $ HH.ClassName "albums" ]
  [ HH.div
    [ HP.class_ $ HH.ClassName "row"]
    ( map (\(Tuple i a) -> renderAlbumTN i a) $ zip (range 1 (length as)) as )
  ]

albumView :: forall m. Maybe Album -> DateTime -> H.ComponentHTML Action () m
albumView am now = case am of
                   Just a -> HH.div_ [ renderAlbumView a now ]
                   Nothing -> HH.div_ [ noAlbum ]

renderAlbumView :: forall m. Album -> DateTime -> H.ComponentHTML Action () m
renderAlbumView a now =
  case a.albumFormat of
            "AppleMusic" -> discogsView
            "Tidal"      -> testView
            _            -> discogsView
  where
  ttl = replaceAll (Pattern ":") (Replacement "_") <<< replaceAll (Pattern "/") (Replacement "·") $ a.albumArtist <> " - " <> a.albumTitle
  dt = fromRight "???????" <<< formatDateTime "YYMMDD" $ now -- "2022-10-19T20:01"
  dtl = fromRight "???????" <<< formatDateTime "YYYY-MM-DDTHH:mm" $ now -- "2022-10-19T20:01"
  discogsView =
    HH.div
      [ HP.class_ $ HH.ClassName "data-deskgap-drag" ]
      [ HH.h1_ [ HH.text "This is the Clutter App!" ]
      , HH.div
        [ HP.class_ $ HH.ClassName "cover-container"]
        [ HH.div
          [ HP.class_ $ HH.ClassName "cover-img" ] $
          [ HH.a
            [ HP.href a.albumURL] $
            [ HH.img [ HP.src a.albumCover
                     , HP.alt "cover image"
                     --, HP.onerror "this.onerror=null;this.src='http://localhost:8080/no-cover.png';"
                     , HP.class_ $ HH.ClassName "cover-image"
                     ]
            ]
          ] <> renderBadges 0 a
        ]
      -- , renderAlbumTN 0 a
      , HH.p_ [HH.text ("Title: "  <> a.albumTitle)]
      , HH.p_ [HH.text ("Artist: " <> a.albumArtist)]
      , HH.p_ [HH.text ("Year: "   <> a.albumReleased)]
      , HH.br_
      , HH.div
        [ HP.class_ ( HH.ClassName "quoteable" )
        -- , CSS.style do
        --     fontSize $ px 20.0
        --     backgroundColor orange
        ]
        ([ HH.samp_ [ HH.text $ dt <> "-" <> ttl ]
        , HH.br_
        , HH.samp_ [ HH.text "---" ]
        , HH.br_
        , HH.samp_ [ HH.text $ "date: " <> dtl ]
        , HH.br_
        , HH.samp_ [ HH.text ("title: " <> ttl) ]
        , HH.br_
        , HH.samp_ [ HH.text "---" ]
        , HH.br_
        , HH.samp_ [ HH.text ("### " <> a.albumArtist <> " – " <> a.albumTitle) ]
        , HH.br_
        , HH.samp_ [ HH.text ("[![](" <> a.albumCover <> ")][1] ") ]
        , HH.br_
        -- reference-style link to album page
        , HH.br_
        , HH.samp_ [ HH.text ("[1]: " <> a.albumURL) ]
        ]
        <> case a.albumAMusic of
                Nothing -> []
                Just amid ->  [ HH.br_
                              , if S.take 2 amid == "l."
                                  then HH.samp_ [ HH.text ("[2]: " <> "https://music.apple.com/library/albums/" <> amid) ]
                                  else HH.samp_ [ HH.text ("[2]: " <> "https://music.apple.com/us/album/" <> amid) ]
                              ]
        <> case a.albumTidal of
                Nothing -> []
                Just tid ->  [ HH.br_
                              , HH.samp_ [ HH.text ("[3]: " <> "https://listen.tidal.com/album/" <> tid) ]
                              ]
        -- icon with link to album page
        <> [ HH.br_
            , HH.br_
            ]
        <> case a.albumAMusic of
                Nothing -> []
                Just _ -> [ HH.samp_ [ HH.text "[![[attachments/am-is.png]]][2]"  ]
                          ]
        <> case a.albumTidal of
                Nothing -> []
                Just _ -> [ HH.samp_ [ HH.text "[![[attachments/tidal-is.png]]][3]" ] ]
        -- embeded player for album
        <> [ HH.br_ ]
        <> case a.albumAMusic of
                Nothing -> []
                Just amid -> [ HH.br_
                              , HH.samp_ [ HH.text $ "<iframe allow=\"autoplay *; encrypted-media *; fullscreen *\" frameborder=\"0\" height=\"450\" style=\"width:100%;max-width:660px;overflow:hidden;background:transparent;\" sandbox=\"allow-forms allow-popups allow-same-origin allow-scripts allow-storage-access-by-user-activation allow-top-navigation-by-user-activation\" src=\"https://embed.music.apple.com/us/album/turn-blue/" <> amid <> "\"></iframe>" ]
                              ]
        <> case a.albumTidal of
                Nothing -> []
                Just tid -> [ HH.br_
                            , HH.samp_ [ HH.text $ "<div style=\"position: relative; padding-bottom: 100%; height: 0; overflow: hidden; max-width: 100%;\"><iframe src=\"https://embed.tidal.com/albums/" <> tid <> "?layout=gridify\" frameborder= \"0\" allowfullscreen style=\"position: absolute; top: 0; left: 0; width: 100%; height: 1px; min-height: 100%; margin: 0 auto;\"></iframe></div>" ]
                ]

        )
      ]
  testView =
    HH.div
      [ HP.class_ $ HH.ClassName "data-deskgap-drag" ]
      [ HH.iframe
        [ HP.src ("http://localhost:8080/album/" <> show a.albumID)
        , HP.title "iframe_a"
        , HP.style "height:600px;width:100%;"
        -- , frameborder "0"
        -- , allow "autoplay *; encrypted-media *; fullscreen *"
        ]
      ]
-- allow :: forall r i. String -> HH.IProp ( allow :: String | r ) i
-- allow = HH.prop (HH.PropName "allow")
-- frameborder :: forall r i. String -> HH.IProp ( frameborder :: String | r ) i
-- frameborder = HH.prop (HH.PropName "frameborder")

noAlbum :: forall w i. HH.HTML w i
noAlbum =
  HH.div
    [ HP.id "root" ]
    [
      HH.h1 [ ] [ HH.text "This is the Clutter App!" ]
    , HH.div
      [ HP.class_ $ HH.ClassName "login-message" ]
      [ HH.span_
        [ HH.text "Unknown album, sorry! Please see "
        , HH.a
          [HP.href "http://localhost:8080/albums/All"]
          [HH.text "all Albums" ]
        ]
      ]
    ]


renderBadges :: forall m. Int -> Album -> Array (H.ComponentHTML Action () m)
renderBadges idx a =
  let uhq = ""
      ln = "Basement"
  in
  [ HH.div [ HP.class_ $ HH.ClassName "idx" ]
    [
      -- L.a_ [L.href_ ("http://lmini.local:8080/album/" <> show (albumID a))] $
      -- L.a_ [L.href_ (albumURL a)] $
      HH.text $ " " <> show idx <> " "
    ]
  , case a.albumFormat of
                  "Vinyl" ->
                    HH.div
                    [ HP.class_ $ HH.ClassName "cover-obackground" ]
                    [ HH.a
                      [ HP.href a.albumURL]
                      [ HH.span
                        [ HP.class_ $ HH.ClassName "fas fa-record-vinyl fa-sm" ]
                        [ HH.text "" ]
                      ]
                    -- HH.img [ HP.src_ "http://localhost:8080/discogs-icon.png", HH.alt "D", HP.class_ "cover-oimage" ]
                    ]
                  "Tidal" -> HH.div_ []
                  "AppleMusic" ->
                    HH.div
                    [ HP.class_ $ HH.ClassName "cover-obackground" ]
                    [ HH.a
                      [ HP.href a.albumURL]
                      [ HH.img
                        [ HP.src "http://localhost:8080/am-icon.png"
                        , HP.alt "A"
                        , HP.class_ $ HH.ClassName "cover-oimage"
                        ]
                      ]
                    ]
                  "CD" ->
                    HH.div
                    [ HP.class_ $ HH.ClassName "cover-obackground" ]
                    [ HH.a
                      [ HP.href a.albumURL]
                      [ HH.span
                        [ HP.class_ $ HH.ClassName "fas fa-compact-disc fa-sm" ]
                        [ HH.text "" ]
                      ]
                    ]
                  "CD, Box Set" ->
                    HH.div
                    [ HP.class_ $ HH.ClassName "cover-obackground" ]
                    [ HH.a [ HP.href a.albumURL ]
                      [ HH.span
                        [ HP.class_ $ HH.ClassName "fa-stack fa-sm" ]
                        [ HH.span
                          [ HP.class_ $ HH.ClassName "fa fa-square-o fa-stack-1x" ]
                          [ HH.text "" ]
                        , HH.span
                          [ HP.class_ $ HH.ClassName "fa fa-compact-disc fa-stack-1x" ]
                          [ HH.text "" ]
                        ]
                      ]
                    ]
                  "Box Set, Vinyl" ->
                    HH.div
                    [ HP.class_ $ HH.ClassName "cover-obackground" ]
                    [ HH.a [ HP.href a.albumURL] [ HH.span
                        [ HP.class_ $ HH.ClassName "far fa-clone fa-sm" ]
                        [ HH.text "" ] ] ]
                  "Vinyl, Box Set" ->
                    HH.div
                    [ HP.class_ $ HH.ClassName "cover-obackground" ]
                    [ HH.a [ HP.href a.albumURL] [ HH.span
                        [ HP.class_ $ HH.ClassName "far fa-clone fa-sm" ]
                        [ HH.text "" ] ] ]
                  "Vinyl, Vinyl" ->
                    HH.div
                    [ HP.class_ $ HH.ClassName "cover-obackground" ]
                    [ HH.a [ HP.href a.albumURL] [ HH.span
                        [ HP.class_ $ HH.ClassName "far fa-clone fa-sm" ]
                        [ HH.text "" ] ] ]
                  "Files" ->
                    HH.div
                    [ HP.class_ $ HH.ClassName "cover-obackground" ]
                    [ HH.a [ HP.href a.albumURL] [ HH.span
                        [ HP.class_ $ HH.ClassName "far fa-file-audio fa-sm" ]
                        [ HH.text "" ] ] ]
                  "Streaming" ->
                    HH.div
                    [ HP.class_ $ HH.ClassName "cover-obackground" ]
                    [ HH.a
                      [ HP.href a.albumURL]
                      [ HH.span
                        [ HP.class_ $ HH.ClassName "far fa-wifi fa-sm" ]
                        [ HH.text "" ]
                      ]
                    ]
                  _ ->
                    HH.div
                      [ HP.class_ $ HH.ClassName "cover-obackground"]
                      [ HH.a [ HP.href a.albumURL ] [ HH.text a.albumFormat ]
                      ]
  , case a.albumTidal of
              Nothing -> HH.div_ []
              Just tid -> HH.div
                [ HP.class_ $ HH.ClassName "cover-obackground1" ]
                [ HH.a
                  [ HP.href ("https://listen.tidal.com/album/" <> tid)]
                  [ HH.img
                    [ HP.src "http://localhost:8080/tidal-icon.png"
                    , HP.alt "T"
                    , HP.class_ $ HH.ClassName "cover-oimage"
                    ]
                  ]
                ]
  , case a.albumAMusic of
              Nothing -> HH.div_ []
              Just amid -> HH.div
                [ HP.class_ $ HH.ClassName "cover-obackground3" ]
                [ HH.a
                  [ HP.href if S.take 2 amid == "l."
                        then "https://music.apple.com/library/albums/" <> amid
                        else "https://music.apple.com/us/album/" <> amid
                  ]
                  [ HH.img
                    [ HP.src "http://localhost:8080/am-icon.png"
                    , HP.alt "A"
                    , HP.class_ $ HH.ClassName "cover-oimage"
                    ]
                  ]
                ]
  , case a.albumRating of
                1 ->  HH.div
                        [ HP.class_ $ HH.ClassName "rat" ]
                        [ HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ]
                               [ HH.text "" ]
                        , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                               , HP.style "color:lightgray" ]
                               [ HH.text "" ]
                        , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                               , HP.style "color:lightgray" ]
                               [ HH.text "" ]
                        , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                               , HP.style "color:lightgray" ]
                               [ HH.text "" ]
                        , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                               , HP.style "color:lightgray" ]
                               [ HH.text "" ]
                        ]
                2 ->  HH.div
                        [ HP.class_ $ HH.ClassName "rat" ]
                        [ HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ]
                               [ HH.text "" ]
                        , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ]
                               [ HH.text "" ]
                        , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                               , HP.style "color:lightgray" ]
                               [ HH.text "" ]
                        , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                               , HP.style "color:lightgray" ]
                               [ HH.text "" ]
                        , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                               , HP.style "color:lightgray" ]
                               [ HH.text "" ]
                        ]
                3 ->  HH.div
                        [ HP.class_ $ HH.ClassName "rat" ]
                        [ HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ]
                               [ HH.text "" ]
                        , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ]
                               [ HH.text "" ]
                        , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ]
                               [ HH.text "" ]
                        , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                               , HP.style "color:lightgray" ]
                               [ HH.text "" ]
                        , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                               , HP.style "color:lightgray" ]
                               [ HH.text "" ]
                        ]
                4 ->  HH.div
                        [ HP.class_ $ HH.ClassName "rat" ]
                        [ HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ]
                               [ HH.text "" ]
                        , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ]
                               [ HH.text "" ]
                        , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ]
                               [ HH.text "" ]
                        , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ]
                               [ HH.text "" ]
                        , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                               , HP.style "color:lightgray" ]
                               [ HH.text "" ]
                        ]
                5 ->  HH.div
                        [ HP.class_ $ HH.ClassName "rat" ]
                        [ HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ]
                               [ HH.text "" ]
                        , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ]
                               [ HH.text "" ]
                        , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ]
                               [ HH.text "" ]
                        , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ]
                               [ HH.text "" ]
                        , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ]
                               [ HH.text "" ]
                        ]
                _ -> HH.div_ []
  , if a.albumPlays > 0 then
              HH.div [ HP.class_ $ HH.ClassName "plays"]
              [ case a.albumPlays of
              cnt | cnt == 1 -> HH.span_
                    [ HH.i
                      [ HP.class_ $ HH.ClassName "far fa-check-circle" ]
                      [ HH.text "" ]
                    ]
                  | cnt == 2 -> HH.span_
                    [ HH.i
                      [ HP.class_ $ HH.ClassName "fa fa-thermometer-1" ]
                      [ HH.text "" ]
                    ]
                  | cnt == 3 -> HH.span_
                    [ HH.i
                      [ HP.class_ $ HH.ClassName "fa fa-thermometer-2"
                      , HP.style "color:orange" ]
                      [ HH.text "" ]
                    ]
                  | cnt == 4 -> HH.span_
                    [ HH.i
                      [ HP.class_ $ HH.ClassName "fa fa-thermometer-3"
                      , HP.style "color:red" ]
                      [ HH.text "" ]
                    ]
                  | cnt >= 5 -> HH.span_
                    [ HH.i
                      [ HP.class_ $ HH.ClassName "fa fa-thermometer-4"
                      , HP.style "color:red" ]
                      [ HH.text "" ]
                    ]
              _ ->  HH.div_ []
              , HH.span
                  [ HP.class_ $ HH.ClassName "loctext" ]
                  [ HH.text $ "Played " <> show a.albumPlays <> " times" ]
              ]
            else HH.div_ []
  , case a.albumLocation of
                Just loc ->
                  HH.div
                    [ HP.class_ $ HH.ClassName "cover-obackground2" ]
                    [ HH.a
                      [ HP.href (uhq <> loc <> "?sortBy=Default&sortOrder=" <> show Asc)
                      ]
                      [ HH.i
                        [ HP.class_ $ HH.ClassName "fa fa-barcode"
                        , HP.style "color:red"
                        ] [ HH.text "" ]
                      ]
                    , HH.span
                      [ HP.class_ $ HH.ClassName "loctext" ]
                      [ HH.text "Location: "
                      , HH.a
                          [ HP.class_ $ HH.ClassName "loclink"
                          , HP.href (uhq <> loc <> "?sortBy=Default&sortOrder=" <> show Asc)
                          ]
                          [ HH.text if loc == ln
                                        then loc <> " #" <> show idx
                                        else loc
                          ]
                      ]
                    ]
                Nothing ->
                  let loc = "Discogs"
                  in case a.albumShelf of
                    Just (Tuple shelf ipos) ->
                      HH.div
                      [ HP.class_ $ HH.ClassName "cover-obackground2" ]
                      [ HH.a
                        [ HP.href (uhq <> loc <> "?sortBy=Default&sortOrder=" <> show Asc)
                        ]
                        [ HH.i
                          [ HP.class_ $ HH.ClassName "fa fa-barcode"
                          , HP.style "color:black"
                          ] [ HH.text "" ]
                        ]
                      , HH.span
                        [ HP.class_ $ HH.ClassName "loctext" ]
                        [ HH.text $ "Location: " <> shelf <> " #" <> show ipos
                        , HH.a
                            [ HP.class_ $ HH.ClassName "loclink"
                            , HP.href (uhq <> loc <> "?sortBy=Default&sortOrder=" <> show Asc)
                            ]
                            [ HH.text if loc == ln
                                          then loc <> " #" <> show idx
                                          else loc
                            ]
                        ]
                      ]
                    Nothing -> HH.div [] []
                --   case M.lookup (albumID a) (locs envr) of
                --     Just (loc, pos) ->
                --       L.div_ [L.class_ "cover-obackground2"] $ do
                --         L.a_ [L.href_ (uhq <> loc <> "?sortBy=Default&sortOrder=" <> show Asc)] $
                --           -- L.i_ [ L.class_ "fa fa-align-justify fa-rotate-90" ] ""
                --           L.i_ [ L.class_ "fa fa-barcode" ] ""
                --         L.span_ [L.class_ "loctext"] $ do
                --           "Location: "
                --           L.a_ [L.class_ "loclink", L.href_ (uhq <> loc <> "?sortBy=Default&sortOrder=" <> show Asc)] $
                --             L.toHtml $ loc <> " #" <> show pos
                --     _ -> ""

  ]
