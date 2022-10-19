module RenderAlbumView (
  albumComponent
  ) where

import Prelude
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Effect.Aff (Aff)

import Types (Album)

noAlbum :: forall w i. HH.HTML w i
noAlbum =
  HH.div
    [ HP.id "root" ]
    [
      HH.h1 [ ] [ HH.text "This is the Clutter App!" ]
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
        [ HH.text "Unknown album, sorry! Please see "
        , HH.a
          [HP.href "http://localhost:8080/albums/All"]
          [HH.text "all Albums" ]
        ]
      ]
    ]

albumElement :: forall w i. Album -> HH.HTML w i
albumElement { albumID: aid, albumFormat: af, albumCover: ac } =
  case af of
            "AppleMusic" -> appleMusicView
            "Tidal" -> tidalView
            _ -> discogsView
  where
  appleMusicView = discogsView
  tidalView = discogsView

  discogsView =
    HH.div
      [ HP.id "root" ]
      [
        HH.h1 [ ] [ HH.text "This is the Clutter App!" ]
      , HH.div
        [ HP.class_ $ HH.ClassName "login-message" ]
        [ HH.span_
          [ HH.text "Great album! Have a look on Clutter "
          , HH.a
            [ HP.href $ "http://localhost:8080/album/" <> show aid ]
            [ HH.text "here" ]
          ]
        ]
      ]

albumView :: forall i w. Maybe Album -> HH.HTML w i
albumView am = case am of
                   Just a -> HH.div_ [ albumElement a ]
                   Nothing -> HH.div_ [ noAlbum ]
  -- H.mkComponent
  -- { initialState: const 0
  -- , render: render am
  -- , eval :  H.mkEval $ H.defaultEval { handleAction = handleAction }
  -- }
  -- where
    -- render am' _ = case am' of
    --                Just a -> HH.div_ [ albumElement a ]
    --                Nothing -> HH.div_ [ noAlbum ]


type State = Maybe Album
data Action = Increment | Decrement

albumComponent :: forall query input output m. Maybe Album -> H.Component query input output m
albumComponent am =
  H.mkComponent
    { initialState: const am
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
initialState :: forall input. input -> State
initialState _ = Nothing

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
    , albumView state
    , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
    ]

handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Decrement ->
    H.modify_ \state -> Nothing
  Increment ->
    H.modify_ \state -> state
