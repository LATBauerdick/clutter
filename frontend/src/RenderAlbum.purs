module RenderAlbum where

import Prelude
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Types (Album)

noAlbum :: forall w i. HH.HTML w i
noAlbum =
  HH.div
    [ HP.id "root" ]
    [
      HH.h1 [ ] [ HH.text "this is the clutter app!" ]
    , HH.input
      [ HP.placeholder "name" ]
    , HH.button
      [ HP.classes [ HH.ClassName "btn-primary" ]
      , HP.type_ HP.ButtonSubmit
      ]
      [ HH.text "submit" ]
    , HH.div
      [ HP.class_ $ HH.ClassName "login-message" ]
      [ HH.span_
        [ HH.text "unknown album, sorry! please see all albums on "
        , HH.a
          [HP.href "http://localhost:8080/albums/discogs"]
          [HH.text "discogs" ]
        ]
      ]
    ]

element :: forall w i. HH.HTML w i
element =
  HH.div
    [ HP.id "root" ]
    [
      HH.h1 [ ] [ HH.text "this is the clutter app!" ]
    , HH.div
      [ HP.class_ $ HH.ClassName "login-message" ]
      [ HH.span_
        [ HH.text "Great album! Please see all albums on "
        , HH.a
          [HP.href "http://localhost:8080/albums/discogs"]
          [HH.text "discogs" ]
        ]
      ]
    ]

albumView :: forall q i o m. Maybe Album -> H.Component q i o m
albumView am = H.mkComponent
  { initialState: const Nothing
  , render
  , eval :  H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
    render am = case am of
                   Just _ -> HH.div_ [ element ]
                   Nothing -> HH.div_ [ noAlbum ]
    handleAction = case _ of
      _ -> H.modify_ \_ -> am

