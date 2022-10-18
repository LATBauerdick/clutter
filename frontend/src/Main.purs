module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson)

import Effect (Effect)
-- import Effect.Class (liftEffect)
import Effect.Class.Console as Console
-- import Effect.Exception (throw)

-- import Effect.Aff (launchAff_)

-- import Web.DOM.NonElementParentNode (getElementById)
-- import Web.HTML (window)
-- import Web.HTML.HTMLDocument (toNonElementParentNode)
-- import Web.HTML.Window (document)


import GetStuff (getUrl, _encodeURIComponent)
import Types (Album)

-- import React.Basic.DOM (div, text, h3_, render) as RD
-- import React.Basic.Hooks (ReactComponent, reactComponent, element) as RH

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Counter (counter)
import RenderAlbum (albumView)

-- mkClutterApp :: Effect (RH.ReactComponent {})
-- mkClutterApp = do
--   RH.reactComponent "ClutterApp" \_ -> React.do
--     pure
--     $ RD.div
--     { className: "container"
--     , children: [ RD.div
--                 { className: "row"
--                 , children:
--                   [ RD.h3_ [ RD.text "Hi - this is the ClutterApp!" ] ]
--                 }
--       ]
--     }

type J0 = { data :: { id :: Int
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

main :: Effect Unit
-- main = launchAff_ do
main = HA.runHalogenAff do
-- main = do
  Console.log "🍝 Rendering clutter component"
  Console.log $ _encodeURIComponent "🍝 Rendering clutter component"

  str0 <- getUrl "https://reqres.in/api/users/1"
  Console.logShow ((decodeJson =<< parseJson str0) :: Either JsonDecodeError J0)

  str <- getUrl "http://localhost:8080/albumj/659642"
  let decodedStr = (decodeJson =<< parseJson str) :: Either JsonDecodeError AlbumJ
  let aje :: Either JsonDecodeError AlbumJ
      aje = decodedStr
  Console.logShow $ aje
  let a = case aje of --case (decodeJson =<< parseJson str) :: Either JsonDecodeError AlbumJ of
                    Right { aid: _, album: a } -> Just a -- { _, a } -> Just a
                    Left err -> Nothing

  body <- HA.awaitBody
  runUI (albumView a) unit body


  -- ctr <- liftEffect $ window >>= document >>= toNonElementParentNode >>> getElementById "container"
  -- case ctr of
  --   Nothing -> liftEffect $ throw "Container element not found."
  --   Just c -> do
  --     Console.log "CLUTTER!!"
  --     clutterApp <- liftEffect $ mkClutterApp
  --     let app = RH.element clutterApp {}
  --     liftEffect $ RD.render app c
