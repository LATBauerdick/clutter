module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either)

import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson)

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (throw)

import Effect.Aff (launchAff_)

import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

import React.Basic.DOM as D
import React.Basic.Hooks (ReactComponent, reactComponent, element)

import GetStuff (getUrl, _encodeURIComponent)

mkClutterApp :: Effect (ReactComponent {})
mkClutterApp = do
  reactComponent "ClutterApp" \_ -> R.do
    pure
    $ D.div
    { className: "container"
    , children: [ D.div
                { className: "row"
                , children:
                  [ D.h3_ [ D.text "Hi - this is the ClutterApp!" ] ]
                }
      ]
    }

type J0 = { data ::
            { id :: Int
            , email :: String
            , first_name :: String
            , last_name :: String
            , avatar :: String
            , j1dummy :: Maybe String
            }
          }
-- j0FromJson :: Json -> Either JsonDecodeError J0
-- j0FromJson = decodeJson

type AlbumJ = { aid :: Int
              , album :: Album
              }
type Album =  { albumID :: Int
              , albumAMusic :: Maybe String
              , albumTidal :: Maybe String
              , albumAdded :: Maybe String
              , albumArtist :: String
              , albumTitle :: String
              , albumCover :: String
              , albumURL :: String
              , albumFormat :: Maybe String
              , albumLocation :: Maybe String
              , albumTags :: Array String
              , albumFolder :: Int
              , albumPlays :: Int
              , albumRating :: Int
              , albumReleased :: String
              }

main :: Effect Unit
main = launchAff_ do
-- main = do
  Console.log "🍝 Rendering clutter component"
  Console.log $ _encodeURIComponent "🍝 Rendering clutter component"
  -- str <- getUrl "https://reqres.in/api/users/1"
  -- let decodedStr = (decodeJson =<< parseJson str) :: Either JsonDecodeError J0
  str <- getUrl "http://localhost:8080/albumj/659642"
  let decodedStr = (decodeJson =<< parseJson str) :: Either JsonDecodeError AlbumJ
  Console.logShow $ decodedStr

  Console.log str
  ctr <- liftEffect $ window >>= document >>= toNonElementParentNode >>> getElementById "container"
  case ctr of
    Nothing -> liftEffect $ throw "Container element not found."
    Just c -> do
      Console.log "CLUTTER!!"
      clutterApp <- liftEffect $ mkClutterApp
      let app = element clutterApp {}
      liftEffect $ D.render app c
