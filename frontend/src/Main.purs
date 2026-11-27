module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (fst, snd)
import Data.Set as Set

import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson)

import Effect.Aff (Aff)
import Effect (Effect)
import Effect.Class.Console as Console
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import GetStuff (getUrl, getNow)
import Types (AlbumsJ, State, AlbumList(..), MenuState, MenuParams, ParamsJ, SortOrder(..))

import AlbumComponent (aComponent)

baseUrl :: String
baseUrl = "/"

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
  let apiBaseUrl :: String
      apiBaseUrl = baseUrl <> "api/"

  sjs <- getUrl $ apiBaseUrl <> "paramsq/all"
  let sje = (decodeJson =<< parseJson sjs) :: Either JsonDecodeError ParamsJ
  Console.logShow sje
  let mps = case sje of
                    Right { params: ps } -> ps
                    Left _ -> defaultMenuParams

  -- the start list, unless the browser remembers the last view (AlbumComponent, LoadNodeName)
  let ln = "Collection"
  r <- getUrl (apiBaseUrl <> "albumsq/" <> ln)
  let lje = (decodeJson =<< parseJson r) :: Either JsonDecodeError AlbumsJ
  let lss = case lje of
                      Right { lalbums: ls' } -> ls'
                      Left _ -> []

      ls = map (\a -> (fst a) { albumShelf = (snd a) } ) lss

  let ms :: MenuState
      ms = { sortName : "Default"
           , ln : ln
           , ffs : [] -- [ Tuple "folder.pop" false, Tuple "format.vinyl" true, Tuple "played.never" true ]
           , sso : Asc
           , params : mps
           }

  pure  { apiUrl : apiBaseUrl
        , listName: AlbumList (Just ln)
        , albumList : ls
        , menuOpen: Nothing
        , review: Nothing
        , playFiles: false
        , selecting: false
        , selected: Set.empty
        , notice: Nothing
        , confirmDrop: false
        , sideOpen: false
        , listenedOpen: false
        , sortKept: Nothing
        , search: ""
        , tagFilter: ""
        , streamRef: ""
        , refsX: Nothing
        , findQ: ""
        , found: []
        , curated: []
        , views: []
        , pos: 0
        , album: Nothing
        , albumX: Nothing
        , albumT: Nothing
        , tracksLoading: false
        , version: "", verX: Nothing, verA: Nothing
        , aboutOpen: false
        , aboutLong: false
        , loading: false
        , albumID: "0"
        , now: now
        , result: Nothing
        , nodeMsg: Nothing
        , nodeName: "Node"
        , menu : ms { params { muhq = baseUrl } }
        }

defaultMenuParams :: MenuParams
defaultMenuParams =
  { muhq : baseUrl
  , msorts : [ "Added", "Artist", "Default", "Title" ]
  , msts : [ ] -- sorted tags
  , mlistNames : [ ]
  , mlocNames : [ ]
  , mjellyfin : ""
  }


