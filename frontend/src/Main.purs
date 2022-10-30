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

import GetStuff (getUrl, getNow, _encodeURIComponent)
import Types (AlbumJ, State, MenuState, MenuParams, ParamsJ, SortOrder(..))

import AlbumComponent (aComponent)

main :: Effect Unit
main = HA.runHalogenAff do
  Console.log "🍝 Rendering clutter component"
  Console.log $ _encodeURIComponent "🍝 Rendering clutter component"

  is <- initialState

  body <- HA.awaitBody

  runUI ( aComponent is ) unit body

initialState :: Aff State
initialState = do -- should eventually be saved in preferences
  let initialAlbumID = 659642 -- 19722988
  str <- getUrl $ "http://localhost:8080/albumq/" <> show initialAlbumID
  let aje :: Either JsonDecodeError AlbumJ
      aje = (decodeJson =<< parseJson str) -- :: Either JsonDecodeError AlbumJ
  let am = case aje of
                    Right { album: a } -> Just a
                    Left _ -> Nothing
  now <- getNow
  sjs <- getUrl $ "http://localhost:8080/paramsq/all"
  let sje = (decodeJson =<< parseJson sjs) :: Either JsonDecodeError ParamsJ
  Console.logShow sje
  let mps = case sje of
                    Right { params: ps } -> ps
                    Left _ -> initialMenuParams
  -- Console.logShow aje
  -- Console.logShow now
  let ms :: MenuState
      ms = { sortName : "Default"
           , ln : "Discogs" -- list
           , ffs : [ "pop" ]
           , sso : Asc
           , params : mps
           }

  pure { album: am, loading: false, albumID: show initialAlbumID, now: now, result: Nothing, menu : ms }

initialMenuParams :: MenuParams
initialMenuParams =
  { muhq : "localhost:8080/albums/"
  , msorts : [ "Added", "Artist", "Default", "Title" ]
  , msts : [ "folder.cd", "folder.pop", "genre.classical", "genre.opera", "rated.*****" ] -- sorted tags
  , mlistNames : [ "2022 Listened", "All", "Apple Music",  "Discogs", "Pop", "Tidal" ]
  , mlocNames : [ "Cube A0", "Cube B0 Pop", "Cube E0 Incoming", "Cube E1 Incoming", "Shelf A1 Opera" ]
  }


