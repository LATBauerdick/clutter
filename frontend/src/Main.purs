module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (throw)

import Effect.Aff (launchAff_)
import Effect.Aff.Class (liftAff)

import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

import React.Basic.DOM as D
import React.Basic.Hooks (ReactComponent, reactComponent, element, useState)

import GetStuff (getUrl)

mkClutterApp :: Effect (ReactComponent {})
mkClutterApp = do
  reactComponent "ClutterApp" \props -> R.do
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

main :: Effect Unit
main = launchAff_ do
-- main = do
  Console.log "🍝 Rendering clutter component"
  str <- getUrl "https://reqres.in/api/users/1"
  Console.log str
  ctr <- liftEffect $ window >>= document >>= toNonElementParentNode >>> getElementById "container"
  case ctr of
    Nothing -> liftEffect $ throw "Container element not found."
    Just c -> do
      Console.log "CLUTTER!!"
      clutterApp <- liftEffect $ mkClutterApp
      let app = element clutterApp {}
      liftEffect $ D.render app c
