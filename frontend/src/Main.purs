module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson)

import Effect (Effect)
import Effect.Class.Console as Console

import GetStuff (getUrl, _encodeURIComponent)
import Types (Album)

import Halogen as H
import Halogen.HTML as HH
import Halogen.Aff as HA
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import RenderAlbumView (albumView)

type J0 = { data :: { id :: Int
                    , email :: String
                    , first_name :: String
                    , last_name :: String
                    , avatar :: String
                    , j1dummy :: Maybe String
                    }
          }

type AlbumJ = { aid :: Int
              , album :: Album
              }

main :: Effect Unit
main = HA.runHalogenAff do
  Console.log "🍝 Rendering clutter component"
  Console.log $ _encodeURIComponent "🍝 Rendering clutter component"

  str0 <- getUrl "https://reqres.in/api/users/1"
  Console.logShow ((decodeJson =<< parseJson str0) :: Either JsonDecodeError J0)

  str <- getUrl "http://localhost:8080/albumj/659642"
  let aje :: Either JsonDecodeError AlbumJ
      aje = (decodeJson =<< parseJson str) -- :: Either JsonDecodeError AlbumJ
  let am = case aje of
                    Right { aid: _, album: a } -> Just a
                    Left _ -> Nothing
  Console.logShow aje

  body <- HA.awaitBody
  runUI ( component am ) unit body

type State = Maybe Album
data Action = Increment | Decrement

component :: forall query input output m. Maybe Album -> H.Component query input output m
component am =
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
