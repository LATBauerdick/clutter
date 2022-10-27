module Render (
  render
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), fromRight)
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

import Types (Album, State, MenuState, Action(..))
import RenderTopMenu (renderTopMenu)

render :: forall m. State -> H.ComponentHTML Action () m
render state = do
  HH.div_
    [
      renderTopMenu state.menu
    , HH.h1_ [ HH.text "This it the Clutter App!" ]
    , HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
    , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
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
      , HH.p_
          [ HH.text if state.loading then "Working..." else "" ]
      , HH.div_
          case state.result of
            Nothing -> []
            Just res ->
              [ HH.h2_
                  [ HH.text "Response:" ]
              , HH.pre_
                  [ HH.code_ [ HH.text res ] ]
              ]
      ]
    , albumView state.album state.now
    ]

albumView :: forall i w. Maybe Album -> DateTime -> HH.HTML w i
albumView am now = case am of
                   Just a -> HH.div_ [ renderAlbumView a now ]
                   Nothing -> HH.div_ [ noAlbum ]

renderAlbumView :: forall w i. Album -> DateTime -> HH.HTML w i
renderAlbumView a now =
  case a.albumFormat of
            "AppleMusic" -> appleMusicView
            "Tidal" -> tidalView
            _ -> discogsView
  where
  appleMusicView = discogsView
  tidalView = discogsView

  ttl = replaceAll (Pattern ":") (Replacement "_") <<< replaceAll (Pattern "/") (Replacement "·") $ a.albumArtist <> " - " <> a.albumTitle
  dt = fromRight "???????" <<< formatDateTime "YYMMDD" $ now -- "2022-10-19T20:01"
  dtl = fromRight "???????" <<< formatDateTime "YYYY-MM-DDTHH:mm" $ now -- "2022-10-19T20:01"
  discogsView =
    HH.div
      [ HP.class_ $ HH.ClassName "data-deskgap-drag" ]
      [ HH.div
        [ HP.class_ $ HH.ClassName "cover-container" ]
        [ HH.p_
          [ HH.text "Great album! Have a look on Clutter "
          , HH.a
            [ HP.href $ "http://localhost:8080/album/" <> show a.albumID ]
            [ HH.text "here" ]
          ]
        , HH.a
          [ HP.href a.albumURL]
          [ HH.img [ HP.src a.albumCover
                   , HP.alt "cover image"
                   --, HP.onerror "this.onerror=null;this.src='/no-cover.png';"
                   , HP.class_ $ HH.ClassName "cover-image"
                   ]
          ]
        ]
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
      , HH.iframe
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

