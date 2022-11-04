module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson)

import Effect.Aff (Aff)
import Effect (Effect)
import Effect.Class.Console as Console
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import GetStuff (getUrl, getNow)
import Types (AlbumsJ, State, AlbumList(..), MenuState, MenuParams, ParamsJ, SortOrder(..))

import AlbumComponent (aComponent)

main :: Effect Unit
main = HA.runHalogenAff do
  Console.log "🍝 Starting Clutter App"
  -- Console.log $ _encodeURIComponent "🍝 Rendering clutter component"

  is <- initialState
  body <- HA.awaitBody
  runUI ( aComponent is ) unit body

initialState :: Aff State
initialState = do -- should eventually be saved in preferences
  now <- getNow
  Console.logShow now
  sjs <- getUrl $ "http://localhost:8080/paramsq/all"
  let sje = (decodeJson =<< parseJson sjs) :: Either JsonDecodeError ParamsJ
  let mps = case sje of
                    Right { params: ps } -> ps
                    Left _ -> initialMenuParams

  let ln = "Discogs"
  r <- getUrl ("http://localhost:8080/albumsq/" <> ln)
  let lje = (decodeJson =<< parseJson r) :: Either JsonDecodeError AlbumsJ
  let ls = case lje of
                      Right { lalbums: ls' } -> ls'
                      Left _ -> []

  let ms :: MenuState
      ms = { sortName : "Default"
           , ln : ln
           , ffs : [ "pop" ]
           , sso : Asc
           , params : mps
           }

  pure  { listName: AlbumList (Just ln)
        , albumList : ls
        , album: Nothing
        , loading: false
        , albumID: "0"
        , now: now
        , result: Nothing
        , menu : ms { params { muhq = "http://localhost:8080/albums/" } }
        }

initialMenuParams :: MenuParams
initialMenuParams =
  { muhq : "localhost:8080/albums/"
  , msorts : [ "Added", "Artist", "Default", "Title" ]
  , msts : [ ] -- sorted tags
  , mlistNames : [ ]
  , mlocNames : [ ]
  }


