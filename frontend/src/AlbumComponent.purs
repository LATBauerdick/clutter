module AlbumComponent (
  aComponent
  ) where

import Prelude
import Data.Newtype (unwrap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Either (Either(..))
import Data.Map as M
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Effect.Class.Console as Console
import Data.Array (foldMap)
import Halogen as H

import Effect.Aff.Class (class MonadAff)
import Web.Event.Event as Event

import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson)

import Types  (Album, AlbumJ , AlbumsJ, State, AlbumList(..), Action(..), SortOrder(..))
import GetStuff (getUrl, getNow, _encodeURIComponent )
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
  SetFocus fo -> do
    ofo <- H.gets _.menu.ffs -- define what == is for Array of Tuples...
    H.liftAff $ Console.logShow fo
    if ofo == fo then pure unit else H.modify_ _ { menu { ffs = fo } }
    if ofo == fo then pure unit else updateAlbumList
  ToggleFocus f -> do
    ffs <- H.gets _.menu.ffs
    H.liftAff $ Console.logShow f
    let mffs :: M.Map String Boolean
        mffs = M.fromFoldable ffs
    let doAlter :: Maybe Boolean -> Maybe Boolean
        doAlter mv = if mv == Nothing
                          then Just true
                          else if mv == Just true then Just false
                                                  else Nothing
    let nffs :: Array (Tuple String Boolean)
        nffs = M.toUnfoldable $ M.alter doAlter f mffs
    H.modify_ _ { menu { ffs = nffs }}
    updateAlbumList
  SetSort sn -> do
    osn <- H.gets _.menu.sortName
    H.liftAff $ Console.logShow sn
    if osn == sn then pure unit else H.modify_ _ { menu { sortName = sn } }
    if osn == sn then pure unit else updateAlbumList

  SetSortOrder so -> do
    oso <- H.gets _.menu.sso
    if oso == so then pure unit else H.modify_ _ { menu { sso = so } }
    if oso == so then pure unit else updateAlbumList
  ToggleSortOrder -> do
    so <- H.gets _.menu.sso
    let so' = case so of
                  Asc -> Desc
                  _ ->  Asc
    H.liftAff $ Console.logShow so'
    H.modify_ _ { menu { sso =  so'  }}
    updateAlbumList

  SetAlbumID albumID -> do
    now <- H.liftAff getNow
    H.liftAff $ Console.logShow now
    H.modify_ _ { albumID = albumID, now = now, result = Nothing }

  MakeRequest event -> do
    H.liftEffect $ Event.preventDefault event
    H.modify_ _ { listName = AlbumList Nothing, loading = true }
    albumID <- H.gets _.albumID
    now <- H.liftAff getNow
    api <- H.gets _.apiUrl
    r <- H.liftAff $ getUrl (api <> "albumq/" <> albumID)
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
    api <- H.gets _.apiUrl
    r <- H.liftAff $ getUrl (api <> "albumq/" <> aid)
    H.liftAff $ Console.logShow ((decodeJson =<< parseJson r) :: Either JsonDecodeError AlbumJ)
    let aje = (decodeJson =<< parseJson r) :: Either JsonDecodeError AlbumJ
    let am = case aje of
                      Right { aid: _, album: a } -> Just a
                      Left _ -> Nothing
    H.modify_ _ { album = am, now = now, loading = false, result = Just r }

  ShowListSort alist sn so -> do
    H.modify_ _ { listName = alist, loading = true, menu { ln = fromMaybe "Nothing" <<< unwrap $ alist, sso = so, sortName = sn } }
    updateAlbumList
  ShowList alist -> do
    -- H.liftEffect $ Event.preventDefault event
    H.modify_ _ { listName = alist, loading = true, menu { ln = fromMaybe "Nothing" <<< unwrap $ alist } }
    updateAlbumList
  AlbumPlayed aid -> do
    H.modify_ _ { album = Nothing, loading = true, listName = AlbumList Nothing }
    now <- H.liftAff getNow
    H.liftAff $ Console.logShow now
    api <- H.gets _.apiUrl
    r <- H.liftAff $ getUrl (api <> "albump/" <> aid)
    H.liftAff $ Console.logShow ((decodeJson =<< parseJson r) :: Either JsonDecodeError AlbumJ)
    let aje = (decodeJson =<< parseJson r) :: Either JsonDecodeError AlbumJ
    let am = case aje of
                      Right { aid: _, album: a } -> Just a
                      Left _ -> Nothing
    H.modify_ _ { album = am, now = now, loading = false, result = Just r }

  where
    updateAlbumList :: forall output' m'. MonadAff m' => H.HalogenM State Action () output' m' Unit
    updateAlbumList = do
      H.modify_ _ { loading = true }
      aln <- H.gets _.listName
      let AlbumList mln = aln
          ln = fromMaybe "" mln
      so <- H.gets _.menu.sso
      sn <- H.gets _.menu.sortName
      ffs <- H.gets _.menu.ffs
      api <- H.gets _.apiUrl
      let affs = map (\(Tuple a b) -> if b then a else "-" <> a) ffs
      let url = api <> "albumsq/"
              <>  _encodeURIComponent ln
              <> (if contains ( Pattern "?" ) ln  then "" else "?")
              <> "&sortOrder=" <> show so
              <> "&sortBy=" <> sn
              <> foldMap (\f -> "&focus=%23" <> _encodeURIComponent f) affs
      H.liftAff $ Console.log url
      r <- H.liftAff $ getUrl url
      let lje = (decodeJson =<< parseJson r) :: Either JsonDecodeError AlbumsJ
      let lss = case lje of
                        Right { lalbums: ls' } -> ls'
                        Left _ -> []
          ls :: Array Album
          ls = map (\a -> (fst a) { albumShelf = (snd a) } ) lss
      H.modify_ _ { loading = false, albumList = ls }
