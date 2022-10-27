module AlbumComponent (
  aComponent
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Effect.Class.Console as Console

import Halogen as H

import Effect.Aff.Class (class MonadAff)
import Web.Event.Event as Event
import Data.DateTime (DateTime)

import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson)

import Types  ( Album, AlbumJ
              , State, MenuState
              , SortOrder(..), Action(..))
import GetStuff (getUrl, getNow)
import Render (render)

aComponent :: forall query input output m. MonadAff m => Maybe Album -> DateTime -> H.Component query input output m
aComponent am now =
  H.mkComponent
  { initialState: initialState am now
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

initialState :: forall input. Maybe Album -> DateTime -> input -> State
initialState am now _ = { album: am, loading: false, albumID: "659642", now: now, result: Nothing, menu : initialMenuState }

initialMenuState :: MenuState
initialMenuState =
  { uhq : "localhost:8080/albums/"
  , ln : "Discogs" -- list
  , ffs : [ "pop" ]
  , sorts : [ "Added", "Artist", "Default", "Title" ]
  , sortName : "Default"
  , sso : Asc
  , sts : [ "folder.cd", "folder.pop", "genre.classical", "genre.opera", "rated.*****" ] -- sorted tags
  , listNames : [ "2022 Listened", "All", "Apple Music",  "Discogs", "Pop", "Tidal" ]
  , locNames : [ "Cube A0", "Cube B0 Pop", "Cube E0 Incoming", "Cube E1 Incoming", "Shelf A1 Opera" ]
  }


handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Decrement -> do
    H.modify_ _ { album = Nothing }
  Increment ->
    H.modify_ _ { album = Nothing }
  SetAlbumID albumID -> do
    now <- H.liftAff getNow
    H.liftAff $ Console.logShow now
    H.modify_ _ { albumID = albumID, now = now, result = Nothing }
  MakeRequest event -> do
    H.liftEffect $ Event.preventDefault event
    albumID <- H.gets _.albumID
    now <- H.liftAff getNow
    H.modify_ _ { loading = true }
    r <- H.liftAff $ getUrl ("http://localhost:8080/albumj/" <> albumID)
    H.liftAff $ Console.logShow ((decodeJson =<< parseJson r) :: Either JsonDecodeError AlbumJ)
    let aje :: Either JsonDecodeError AlbumJ
        aje = (decodeJson =<< parseJson r) -- :: Either JsonDecodeError AlbumJ
    let am = case aje of
                      Right { aid: _, album: a } -> Just a
                      Left _ -> Nothing

    H.modify_ _ { album = am, loading = false, result = Just r, now = now}
    -- response <- H.liftAff $ AX.get ResponseFormat.string ("https://api.github.com/users/" <> albumID)
    -- H.modify_ _ { loading = false, result = map _.body (hush response) }
