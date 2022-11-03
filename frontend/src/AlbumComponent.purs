module AlbumComponent (
  aComponent
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Effect.Class.Console as Console

import Halogen as H

import Effect.Aff.Class (class MonadAff)
import Web.Event.Event as Event

import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson)

import Types  (AlbumJ , AlbumsJ, State, AlbumList(..), Action(..))
import GetStuff (getUrl, getNow)
import Render (render)

aComponent :: forall query input output m. MonadAff m => State -> H.Component query input output m
aComponent is =
  H.mkComponent
  { initialState: const is
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Decrement -> do
    H.liftAff $ Console.logShow "Decrement!"
    H.modify_ _ { album = Nothing }
  Increment ->
    H.modify_ _ { album = Nothing }

  SetAlbumID albumID -> do
    now <- H.liftAff getNow
    H.liftAff $ Console.logShow now
    H.modify_ _ { albumID = albumID, now = now, result = Nothing }

  MakeRequest event -> do
    H.liftEffect $ Event.preventDefault event
    H.modify_ _ { listName = AlbumList Nothing, loading = true }
    albumID <- H.gets _.albumID
    now <- H.liftAff getNow
    r <- H.liftAff $ getUrl ("http://localhost:8080/albumq/" <> albumID)
    H.liftAff $ Console.logShow ((decodeJson =<< parseJson r) :: Either JsonDecodeError AlbumJ)
    let aje :: Either JsonDecodeError AlbumJ
        aje = (decodeJson =<< parseJson r) -- :: Either JsonDecodeError AlbumJ
    let am = case aje of
                      Right { aid: _, album: a } -> Just a
                      Left _ -> Nothing
    H.modify_ _ { album = am, loading = false, result = Just r, now = now }

  ShowAlbum aid -> do
    H.modify_ _ { album = Nothing, loading = true, listName = AlbumList Nothing }
    now <- H.liftAff getNow
    H.liftAff $ Console.logShow now
    r <- H.liftAff $ getUrl ("http://localhost:8080/albumq/" <> aid)
    H.liftAff $ Console.logShow ((decodeJson =<< parseJson r) :: Either JsonDecodeError AlbumJ)
    let aje = (decodeJson =<< parseJson r) :: Either JsonDecodeError AlbumJ
    let am = case aje of
                      Right { aid: _, album: a } -> Just a
                      Left _ -> Nothing
    H.modify_ _ { album = am, now = now, loading = false, result = Just r }

  ShowList alist -> do
    -- H.liftEffect $ Event.preventDefault event
    H.modify_ _ { listName = alist, loading = true }
    let AlbumList ml = alist
        ln = fromMaybe "" ml
    H.liftAff $ Console.logShow ln

    r <- H.liftAff $ getUrl ("http://localhost:8080/albumsq/" <> ln)
    H.liftAff $ Console.logShow ((decodeJson =<< parseJson r) :: Either JsonDecodeError AlbumsJ)
    let lje = (decodeJson =<< parseJson r) :: Either JsonDecodeError AlbumsJ
    let ls = case lje of
                      Right { lalbums: ls' } -> ls'
                      Left _ -> []

    H.modify_ _ { loading = false, albumList = ls }
