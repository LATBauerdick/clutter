module AlbumComponent (
  aComponent
  ) where

import Prelude
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Effect.Aff.Class (class MonadAff)
import Web.Event.Event (Event)
import Web.Event.Event as Event

import Types (Album)
import GetStuff (getUrl)

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

albumElement :: forall w i. Album -> HH.HTML w i
albumElement { albumID: aid, albumFormat: af, albumCover: _ } =
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

type State =  { album :: Maybe Album
              , loading :: Boolean
              , username :: String
              , result :: Maybe String
              }
data Action = Increment | Decrement | SetUsername String | MakeRequest Event

aComponent :: forall query input output m. MonadAff m => Maybe Album -> H.Component query input output m
aComponent am =
  H.mkComponent
  { initialState: initialState am
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

initialState :: forall input. Maybe Album -> input -> State
initialState am _ = { album: am, loading: false, username: "659642", result: Nothing }

render :: forall m. State -> H.ComponentHTML Action () m
render state = do
  let am = state.album
  HH.div_
    [ albumView am
    , HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
    , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
    , HH.form
      [ HE.onSubmit \ev -> MakeRequest ev ]
      [ HH.h1_ [ HH.text "Look up Album ID" ]
      , HH.label_
          [ HH.div_ [ HH.text "Enter album id:" ]
          , HH.input
              [ HP.value state.username
              , HE.onValueInput \str -> SetUsername str
              ]
          ]
      , HH.button
          [ HP.disabled state.loading
          , HP.type_ HP.ButtonSubmit
          ]
          [ HH.text "Fetch info" ]
      , HH.p_
          [ HH.text $ if state.loading then "Working..." else "" ]
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
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Decrement -> do
    H.modify_ _ { album = Nothing }
  Increment ->
    H.modify_ _ { album = Nothing }
  SetUsername username -> do
    H.modify_ _ { username = username, result = Nothing }
  MakeRequest event -> do
    H.liftEffect $ Event.preventDefault event
    username <- H.gets _.username
    H.modify_ _ { loading = true }
    r <- H.liftAff $ getUrl ("http://localhost:8080/albumj/" <> username)
    H.modify_ _ { loading = false, result = Just r }
    -- response <- H.liftAff $ AX.get ResponseFormat.string ("https://api.github.com/users/" <> username)
    -- H.modify_ _ { loading = false, result = map _.body (hush response) }
