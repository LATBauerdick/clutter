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

import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson)

import Types  (AlbumJ , State, AlbumList(..), Action(..))
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
    H.modify_ _ { album = Nothing }
  Increment ->
    H.modify_ _ { album = Nothing }
  SetAlbumID albumID -> do
    now <- H.liftAff getNow
    H.liftAff $ Console.logShow now
    H.modify_ _ { albumID = albumID, now = now, result = Nothing }
  MakeRequest event -> do
    H.liftEffect $ Event.preventDefault event
    H.modify_ _ { list = AlbumList Nothing, loading = true }
    albumID <- H.gets _.albumID
    now <- H.liftAff getNow
    r <- H.liftAff $ getUrl ("http://localhost:8080/albumq/" <> albumID)
    H.liftAff $ Console.logShow ((decodeJson =<< parseJson r) :: Either JsonDecodeError AlbumJ)
    let aje :: Either JsonDecodeError AlbumJ
        aje = (decodeJson =<< parseJson r) -- :: Either JsonDecodeError AlbumJ
    let am = case aje of
                      Right { aid: _, album: a } -> Just a
                      Left _ -> Nothing
    H.modify_ _ { album = am, loading = false, result = Just r, now = now}
  ShowList event alist -> do
    -- H.liftEffect $ Event.preventDefault event
    H.modify_ _ { list = alist, loading = true }
    let AlbumList ml = alist
    H.liftAff $ Console.logShow ml
    H.modify_ _ { loading = false }
