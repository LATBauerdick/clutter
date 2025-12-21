module Render (
  render
  ) where

import Prelude
-- import CSS (CSS)
-- import CSS as CSS
-- import CSS.Background (backgroundColor)
-- import CSS.Color (Color, rgb)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Either (fromRight)
import Data.Tuple (Tuple(..), snd)
import Data.Array (range, length, replicate, zip)
import Halogen as H
import Halogen.HTML as HH
-- import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import Data.DateTime (DateTime)
import Data.Formatter.DateTime (formatDateTime)
import Data.String (take, indexOf, replaceAll, contains) as S
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))

import Types (Album, State, AlbumList(..), Action(..), SortOrder(..), pLocList)
import RenderTopMenu (renderTopMenu)

render :: forall m0. State -> H.ComponentHTML Action () m0
render state = do
  HH.div_
    [ case state.listName of
        AlbumList Nothing  -> albumView state.album state.now
        _                  -> renderListView
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
      ]
    ]

  where
  albumView :: forall m. Maybe Album -> DateTime -> H.ComponentHTML Action () m
  albumView am now = case am of
                    Just a -> HH.div_ [ renderAlbumView a now
                                      -- , renderPlayedButton a now
                                      ]
                    Nothing -> HH.div_ [ noAlbum ]

  -- renderPlayedButton :: forall m. Album -> DateTime -> H.ComponentHTML Action () m
  -- renderPlayedButton a _ =
  --   HH.div [ HP.class_ $ HH.ClassName "album-view" ]
  --          [ HH.button [ HE.onClick \_ -> AlbumPlayed (show a.albumID) ]
  --                      [ HH.text " Album Played "]
  --          ]

  renderListView :: forall m. H.ComponentHTML Action () m
  renderListView =
    HH.div  [ HP.class_ $ HH.ClassName "albums" ]
            [ HH.div
              [ HP.class_ $ HH.ClassName "row"]
              ( map (\(Tuple i a) -> renderAlbumTN i a) $ zip idxs  as )
            ]
    where as = state.albumList
          idxs = if pLocList state.listName
            then replicate (length as) 0
            else range 1 (length as)

  renderAlbumTN :: forall m. Int -> Album -> H.ComponentHTML Action () m
  renderAlbumTN idx' a =
    let idx = maybe idx' snd a.albumShelf in
    HH.div [ HP.class_ $ HH.ClassName "album-thumb" ]
      [ HH.div [ HP.class_ $ HH.ClassName "cover-container" ] (
          [ HH.div [ HP.class_ $ HH.ClassName "cover-img" ]
              [ HH.button [ HE.onClick \_ -> ShowAlbum (show a.albumID) ]
                [ HH.img [ HP.src a.albumCover
                          , HP.alt "cover image"
                          , HP.class_ $ HH.ClassName "cover-image"
                          ]
                ]
              ]
          ] <> renderBadges idx a
        )
      -- , HH.span [ HP.class_ $ HH.ClassName "hovtext" ]
      --       [ HH.text a.albumTitle
      --       , HH.text a.albumArtist
      --       ]
      , renderHovAlbumInfo a
      , renderAlbumInfo a
      ]

  renderHovAlbumInfo :: forall m. Album -> H.ComponentHTML Action () m
  renderHovAlbumInfo a =
    HH.div [ HP.class_ $ HH.ClassName "hov-album-info" ]
           [ HH.p [ HP.class_ $ HH.ClassName "album-title"]
                  [ HH.text a.albumTitle ]
           , HH.p [ HP.class_ $ HH.ClassName "album-artist"]
                  [ HH.text a.albumArtist ]
           ]

  renderAlbumInfo :: forall m. Album -> H.ComponentHTML Action () m
  renderAlbumInfo a =
    HH.div [ HP.class_ $ HH.ClassName "album-info" ]
           [ HH.p [ HP.class_ $ HH.ClassName "album-title"]
                  [ HH.text a.albumTitle ]
           , HH.p [ HP.class_ $ HH.ClassName "album-artist"]
                  [ HH.text a.albumArtist ]
           ]

  renderAlbumView :: forall m. Album -> DateTime -> H.ComponentHTML Action () m
  renderAlbumView a now =
    case a.albumFormat of
              "AppleMusic" -> discogsView
              "Tidal"      -> tidalView
              _            -> discogsView
    where
    ttl = replaceAll (Pattern ":") (Replacement "_") <<< replaceAll (Pattern "/") (Replacement "·") $ a.albumArtist <> " - " <> a.albumTitle
    dt = fromRight "???????" <<< formatDateTime "YYMMDD" $ now -- "2022-10-19T20:01"
    dtl = fromRight "???????" <<< formatDateTime "YYYY-MM-DDTHH:mm" $ now -- "2022-10-19T20:01"
    discogsView =
      HH.div [ HP.class_ $ HH.ClassName "album-view" ]
        [ HH.div
          [ HP.class_ $ HH.ClassName "cover-container" ]
          [ HH.div [ HP.class_ $ HH.ClassName "cover-img" ] $
            [ HH.a
              [ HP.href a.albumURL] $
              [ HH.img [ HP.src a.albumCover
                      , HP.alt "cover image"
                      , HP.class_ $ HH.ClassName "cover-image"
                      ]
              ]
            ]
            <> renderBadges 0 a
          ]
        -- , renderAlbumTN 0 a
        , HH.p_ [HH.text ("Title: "  <> a.albumTitle)]
        , HH.p_ [HH.text ("Artist: " <> a.albumArtist)]
        , HH.p_ [HH.text ("Year: "   <> a.albumReleased)]
        , HH.br_
        , HH.button [ HE.onClick \_ -> AlbumPlayed (show a.albumID) ]
                    [ HH.text " Album Played "]
        , HH.br_
        , quotableView
        ]
    quotableView =
      HH.div [ HP.class_ ( HH.ClassName "quoteable" )
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

    tidalView = let qry = S.replaceAll ( Pattern " " ) ( Replacement "+" ) $ a.albumArtist <> "+-+" <> a.albumTitle in
      HH.div [ HP.class_ $ HH.ClassName "album-view" ]
        [ HH.p_ [HH.text ("Tidal Album <"  <> show a.albumID <> ">.")]
        , HH.br_
        , HH.text ("Search \"" <> a.albumTitle <> "\" (" <> a.albumReleased <> ") by " <> a.albumArtist <> " on <")
        , HH.a [HP.href $ "https://www.discogs.com/search/?q=" <> qry <> "&type=all"]
               [HH.text "DISCOGS"]
        , HH.text (">")
        , HH.br_
        , HH.div [ HP.class_ $ HH.ClassName "cover-img" ]
                 [ HH.text "After adding the new Album to Discogs, "
                 , HH.button [ HE.onClick \_ -> UpdateDiscogs ]
                             [ HH.text "<Update>"]
                 ]
        , HH.div [ HP.class_ $ HH.ClassName "album-view" ]
           [ HH.button [ HE.onClick \_ -> AlbumPlayed (show a.albumID) ]
                       [ HH.text " Album Played "]
           ]
        , HH.iframe
          [ HP.src ("https://www.discogs.com/search/?q=" <> qry <> "&type=all")
          , HP.title "iframe_a"
          , HP.style "height:600px;width:100%;"
          -- , frameborder "0"
          -- , allow "autoplay *; encrypted-media *; fullscreen *"
          ]
        ]
--L.div_ [L.class_ "login-message"] $ do
-- L.p_ $ "Tidal Album <" <> show (albumID a) <> ">."
-- L.br_ []
-- let qry = T.replace " " "+" $ albumArtist a <> "+-+" <> albumTitle a
-- L.p_ $ do
--   L.toHtml ("Search \"" <> albumTitle a <> "\" by " <> albumArtist a <> " on ")
--   L.a_ [L.href_ $ "https://www.discogs.com/search/?q=" <> qry <> "&type=all"] "DISCOGS"
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
          [ HH.h2 [ ] [ HH.text "...server is working..." ] ]
        ]
      ]

  renderBadges :: forall m. Int -> Album -> Array (H.ComponentHTML Action () m)
  renderBadges idx a =
    [ rbIndex idx
    , rbFormat a
    , rbTidal a
    , rbAMusic a
    , rbJellyfin a
    , rbRating a
    , rbPlays a
    , rbLocation a
    ]

  rbIndex :: forall m. Int -> H.ComponentHTML Action () m
  rbIndex idx =
    HH.div  [ HP.class_ $ HH.ClassName "idx"
            -- , HC.style do
            --     let idxColor :: Color
            --         idxColor = rgb 0xfd 0x7e 0x14
            --     backgroundColor idxColor
            ]
            [ HH.text $ " " <> show idx <> " " ]

  rbFormat :: forall m. Album -> H.ComponentHTML Action () m
  rbFormat a = do
    let f = a.albumFormat
        isInfixOf :: String -> String -> Boolean
        isInfixOf p s = S.indexOf (Pattern p) s /= Nothing
        c
          | f == "CD-R" = 'r'
          | "Vinyl" `isInfixOf` f && "Box Set" `isInfixOf` f = 'b'
          | "Vinyl, Vinly" `isInfixOf` f = 'b'
          | "Vinyl" `isInfixOf` f = 'v'
          | "SACD" `isInfixOf` f = 'g'
          | "CD" `isInfixOf` f && "Box Set" `isInfixOf` f = 'd'
          | "CD" `isInfixOf` f = 'c'
          | "DVD" `isInfixOf` f = 'e'
          | "Hybrid" `isInfixOf` f = 'c'
          | f == "Streaming" = 's'
          | f == "Files" = 'f'
          | f == "Tidal" = 't'
          | f == "AppleMusic" = 'a'
          | otherwise = 'x'

    HH.div [ HP.class_ $ HH.ClassName "cover-obackground" ]
      [ HH.a [ HP.href a.albumURL]
        case c of
          'v' ->
            [ HH.span [ HP.class_ $ HH.ClassName "fas fa-record-vinyl fa-sm" ] [] ]
          't' ->
            [ HH.img
                [ HP.src (state.menu.params.muhq <> "icons/tidal-is.png"), HP.alt "T"
                , HP.class_ $ HH.ClassName "cover-oimage"
                ]
            ]
          'a' ->
            [ HH.img
                [ HP.src (state.menu.params.muhq <> "icons/am-icon.png"), HP.alt "A"
                , HP.class_ $ HH.ClassName "cover-oimage"
                ]
            ]
          'b' ->
            [ HH.span [ HP.class_ $ HH.ClassName "far fa-clone fa-sm" ] [] ]
          'c' ->
            [ HH.img
                [ HP.src (state.menu.params.muhq <> "icons/cd-icon.png"), HP.alt "CD"
                , HP.class_ $ HH.ClassName "cover-oimage"
                ]
            ]
            -- [ HH.span [ HP.class_ $ HH.ClassName "fas fa-compact-disc fa-sm" ] [] ]
          'd' ->
            [ HH.span
                [ HP.class_ $ HH.ClassName "fa-stack fa-sm" ]
                [ HH.span [ HP.class_ $ HH.ClassName "fa fa-square-o fa-stack-1x" ] []
                , HH.span [ HP.class_ $ HH.ClassName "fa fa-compact-disc fa-stack-1x" ] []
                ]
            ]
          'e' ->
            [ HH.img
                [ HP.src (state.menu.params.muhq <> "icons/dvd-icon.png"), HP.alt "T"
                , HP.class_ $ HH.ClassName "cover-oimage"
                ]
            ]
          'f' ->
            [ HH.span [ HP.class_ $ HH.ClassName "far fa-file-audio fa-sm" ] [] ]
          'g' ->
            [ HH.img
                [ HP.src (state.menu.params.muhq <> "icons/sacd-icon.png"), HP.alt "SACD"
                , HP.class_ $ HH.ClassName "cover-oimage"
                ]
            ]
          'r' ->
            [ HH.img
                [ HP.src (state.menu.params.muhq <> "icons/cd-r-icon.png"), HP.alt "CDR"
                , HP.class_ $ HH.ClassName "cover-oimage"
                ]
            ]
          's' ->
            [ HH.span [ HP.class_ $ HH.ClassName "far fa-wifi fa-sm" ] [] ]
          _ ->
            [ HH.text a.albumFormat ]
    ]

  rbTidal :: forall m. Album -> H.ComponentHTML Action () m
  rbTidal a = case a.albumTidal of
    Nothing -> HH.div_ []
    Just tid -> if a.albumFormat == "Tidal" then HH.div_ [] else -- don't show if just Tidal
      HH.div [ HP.class_ $ HH.ClassName "cover-obackground1" ]
      [ HH.a
        -- [ HP.href ("https://listen.tidal.com/album/" <> tid)]
        [ HP.href ("tidal://album/" <> tid)]
        [ HH.img
          [ HP.src (state.menu.params.muhq <> "icons/tidal-is.png")
          , HP.alt "T"
          , HP.class_ $ HH.ClassName "cover-oimage"
          ]
        ]
      ]

  rbAMusic :: forall m. Album -> H.ComponentHTML Action () m
  rbAMusic a = case a.albumAMusic of
    Nothing -> HH.div_ []
    Just amid -> if a.albumFormat == "AppleMusic" then HH.div_ [] else -- don't show if just Apple Music
      HH.div [ HP.class_ $ HH.ClassName "cover-obackground3" ]
      [ HH.a
        [ HP.href if S.take 2 amid == "l."
              then "https://music.apple.com/library/albums/" <> amid
              else "music://music.apple.com/us/album/" <> amid
              -- else "https://music.apple.com/us/album/" <> amid
        ]
        [ HH.img
          [ HP.src (state.menu.params.muhq <> "icons/am-icon.png")
          , HP.alt "A"
          , HP.class_ $ HH.ClassName "cover-oimage"
          ]
        ]
      ]

  rbJellyfin :: forall m. Album -> H.ComponentHTML Action () m
  rbJellyfin a = case a.albumJellyfin of
    Nothing -> HH.div_ []
    Just jid -> if a.albumFormat == "Jellyfin" then HH.div_ [] else -- don't show if just Jellyfin
      HH.div [ HP.class_ $ HH.ClassName "cover-obackground4"]
      [ HH.a
        [ HP.href ("http://umac:8096/web/#/details?id=" <> jid)]
        [ HH.img
            [ HP.src (state.menu.params.muhq <> "icons/jellyfin-icon.png"), HP.alt "J"
            , HP.class_ $ HH.ClassName "cover-oimage"
            ]
        ]
      ]

  rbRating :: forall m. Album -> H.ComponentHTML Action () m
  rbRating a = case a.albumRating of
    1 ->  HH.div [ HP.class_ $ HH.ClassName "rat" ]
            [ HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                    , HP.style "color:lightgray" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                    , HP.style "color:lightgray" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                    , HP.style "color:lightgray" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                    , HP.style "color:lightgray" ] []
            ]
    2 ->  HH.div [ HP.class_ $ HH.ClassName "rat" ]
            [ HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                    , HP.style "color:lightgray" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                    , HP.style "color:lightgray" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                    , HP.style "color:lightgray" ] []
            ]
    3 ->  HH.div [ HP.class_ $ HH.ClassName "rat" ]
            [ HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                    , HP.style "color:lightgray" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                    , HP.style "color:lightgray" ] []
            ]
    4 ->  HH.div [ HP.class_ $ HH.ClassName "rat" ]
            [ HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                    , HP.style "color:lightgray" ] []
            ]
    5 ->  HH.div [ HP.class_ $ HH.ClassName "rat" ]
            [ HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            ]
    _ -> HH.div_ []

  rbPlays :: forall m. Album -> H.ComponentHTML Action () m
  rbPlays a = 
    if a.albumPlays > 0 then
      HH.div [ HP.class_ $ HH.ClassName "plays"]
      [ case a.albumPlays of
      cnt | cnt == 1 -> HH.span_
            [ HH.i [ HP.class_ $ HH.ClassName "far fa-check-circle" ] [] ]
          | cnt == 2 -> HH.span_
            [ HH.i [ HP.class_ $ HH.ClassName "fa fa-thermometer-1" ] [] ]
          | cnt == 3 -> HH.span_
            [ HH.i [ HP.class_ $ HH.ClassName "fa fa-thermometer-2" , HP.style "color:orange" ] [] ]
          | cnt == 4 -> HH.span_
            [ HH.i [ HP.class_ $ HH.ClassName "fa fa-thermometer-3" , HP.style "color:red" ] [] ]
          | cnt >= 5 -> HH.span_
            [ HH.i
              [ HP.class_ $ HH.ClassName "fa fa-thermometer-4" , HP.style "color:red" ] [] ]
      _ ->  HH.div_ []
      , HH.span [ HP.class_ $ HH.ClassName "hovtext" ]
          [ HH.text $ "Played " <> show a.albumPlays <> " times" ]
      ]
    else HH.div_ []

  rbLocation :: forall m. Album -> H.ComponentHTML Action () m
  rbLocation a = case a.albumLocation of
    Just loc ->
      let shelf = "Box CDs" in
      if S.contains (Pattern shelf) $ S.take 7 loc
        then
        -- treat CD Box special: this was a Discogs Release tagged with Loc C<id>
          HH.div [ HP.class_ $ HH.ClassName "cover-obackground2" ]
          [ HH.i [ HP.class_ $ HH.ClassName "fa fa-barcode" , HP.style "color:black" ] []
          , HH.button
            [ HP.class_ $ HH.ClassName "hovtext"
            , HE.onClick \_ -> ShowListSort (AlbumList (Just shelf)) "Default" Asc ]
            [ HH.text $ shelf <> " #" <> show (fromMaybe 0 a.albumLocIdx)
            ]
          ]
        else
        -- treat it as a URL...
          HH.div [ HP.class_ $ HH.ClassName "cover-obackground2" ]
            [ HH.i [ HP.class_ $ HH.ClassName "fa fa-barcode" , HP.style "color:red" ] []
            , HH.span
              [ HP.class_ $ HH.ClassName "hovtext" ] [ HH.text ""
              , HH.a
                  [ HP.class_ $ HH.ClassName "loclink"
                  , HP.href loc
                  ]
                  [ HH.text $ loc
                  ]
              ]
            ]
    Nothing ->
      case a.albumShelf of
        Just (Tuple shelf ipos) ->
          HH.div [ HP.class_ $ HH.ClassName "cover-obackground2" ]
          [ HH.i [ HP.class_ $ HH.ClassName "fa fa-barcode" , HP.style "color:black" ] []
          , HH.button
            [ HP.class_ $ HH.ClassName "hovtext"
            , HE.onClick \_ -> ShowListSort (AlbumList (Just shelf)) "Default" Asc ]
            [ HH.text $ "" <> shelf <> " #" <> show ipos
            ]
          ]
        Nothing ->
          case a.albumLocation of
            Just loc ->
              -- just treat loc as an URL
              HH.div [ HP.class_ $ HH.ClassName "cover-obackground2" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fa fa-barcode" , HP.style "color:red" ] []
                , HH.span
                  [ HP.class_ $ HH.ClassName "hovtext" ] [ HH.text "Location: "
                  , HH.a
                      [ HP.class_ $ HH.ClassName "loclink"
                      , HP.href loc
                      ]
                      [ HH.text $ loc <> " #" <> show 12
                      ]
                  ]
                ]
            Nothing -> HH.div [] []
