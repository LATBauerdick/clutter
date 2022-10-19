module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson)

import Effect (Effect)
import Effect.Class.Console as Console

import GetStuff (getUrl, getNow, _encodeURIComponent)
import Types (AlbumJ)

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import AlbumComponent (aComponent)

type J0 = { data :: { id :: Int
                    , email :: String
                    , first_name :: String
                    , last_name :: String
                    , avatar :: String
                    , j1dummy :: Maybe String
                    }
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
  now <- getNow
  Console.logShow now

  body <- HA.awaitBody
  runUI ( aComponent am now ) unit body

type State =
  { getResponse :: String
  , postInfo :: String
  }

