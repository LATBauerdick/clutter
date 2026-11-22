module AlbumComponent (
  aComponent
  ) where

import Prelude
import Data.Newtype (unwrap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Either (Either(..))
import Data.Map as M
import Data.String (contains, stripPrefix, length)
import Data.String.Pattern (Pattern(..))
import Effect.Class.Console as Console
import Data.Array (foldMap, zipWith)
import Halogen as H

import Effect.Aff.Class (class MonadAff)
import Web.Event.Event as Event

import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson)

import Types  (Album, AlbumJ, AlbumXJ, AlbumsJ, ListenedJ, State, AlbumList(..), Action(..), SortOrder(..))
import GetStuff (getUrl, getNow, _encodeURIComponent )
import Render (render)

aComponent :: forall query input output m. MonadAff m => State -> H.Component query input output m
aComponent is =
  H.mkComponent
  { initialState: const is
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just LoadNodeName }
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
    H.modify_ _ { listName = AlbumList Nothing, albumX = Nothing, loading = true }
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
    loadDetail am

  ShowAlbum aid -> do
    H.modify_ _ { album = Nothing, albumX = Nothing, loading = true, listName = AlbumList Nothing, nodeMsg = Nothing }
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
    loadDetail am

  ShowListSort alist sn so -> do
    H.modify_ _ { listName = alist, loading = true, menu { ln = fromMaybe "Nothing" <<< unwrap $ alist, sso = so, sortName = sn } }
    updateAlbumList
  ShowList alist -> do
    -- H.liftEffect $ Event.preventDefault event
    H.modify_ _ { listName = alist, loading = true, menu { ln = fromMaybe "Nothing" <<< unwrap $ alist } }
    updateAlbumList
  ShowFocus tag -> do
    -- from the album view: all albums with this tag (a genre chip, a tag)
    let alist = AlbumList (Just "All")
    H.modify_ _ { listName = alist, loading = true, menu { ln = "All", ffs = [ Tuple tag true ] } }
    updateAlbumList
  AlbumPlayed aid -> do
    H.modify_ _ { album = Nothing, albumX = Nothing, loading = true, listName = AlbumList Nothing }
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
    loadDetail am
  NodePlay tid mode -> do
    -- the album view stays; only the status line under the button changes
    H.modify_ _ { nodeMsg = Just "Node: starting…" }
    api <- H.gets _.apiUrl
    r <- H.liftAff $ getUrl (api <> "nodeplay/" <> tid <> "?mode=" <> mode)
    let msg = case (decodeJson =<< parseJson r) :: Either JsonDecodeError { reqInfo :: String } of
                Right { reqInfo: i } -> i
                Left _ -> r
    H.liftAff $ Console.log msg
    H.modify_ _ { nodeMsg = Just msg }
  LoadNodeName -> do
    api <- H.gets _.apiUrl
    r <- H.liftAff $ getUrl (api <> "nodeinfo")
    case (decodeJson =<< parseJson r) :: Either JsonDecodeError { reqInfo :: String, reqResult :: Int } of
      Right { reqInfo: n, reqResult: 1 } -> H.modify_ _ { nodeName = n }
      _ -> H.liftAff $ Console.log ("nodeinfo: " <> r) -- keep "Node"
  NodeCtl cmd -> do
    api <- H.gets _.apiUrl
    r <- H.liftAff $ getUrl (api <> "nodectl/" <> cmd)
    let msg = case (decodeJson =<< parseJson r) :: Either JsonDecodeError { reqInfo :: String } of
                Right { reqInfo: i } -> i
                Left _ -> r
    H.liftAff $ Console.log msg
    H.modify_ _ { nodeMsg = Just msg }
  UpdateDiscogs -> do
    H.modify_ _ { album = Nothing, albumX = Nothing, loading = true, listName = AlbumList Nothing }
    now <- H.liftAff getNow
    H.liftAff $ Console.logShow now
    api <- H.gets _.apiUrl
    _ <- H.liftAff $ getUrl (api <> "req?event=update")
    H.liftAff $ Console.logShow "Discogs Updated"
    let alist = AlbumList (Just "Discogs")
    H.modify_ _ { listName = alist, loading = true, menu { ln = fromMaybe "Nothing" <<< unwrap $ alist } }
    updateAlbumList

  where
    -- the album page's details, once the album itself is there; an older backend
    -- without /api/albumxq just leaves them out
    loadDetail :: forall output' m'. MonadAff m' => Maybe Album -> H.HalogenM State Action () output' m' Unit
    loadDetail am = case am of
      Nothing -> pure unit
      Just a -> do
        api <- H.gets _.apiUrl
        r <- H.liftAff $ getUrl (api <> "albumxq/" <> a.albumID)
        cur <- H.gets _.album
        case (decodeJson =<< parseJson r) :: Either JsonDecodeError AlbumXJ, cur of
          Right x, Just c | x.xid == c.albumID -> H.modify_ _ { albumX = Just x }
          _, _ -> pure unit

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
          query = "sortOrder=" <> show so
              <> "&sortBy=" <> sn
              <> foldMap (\f -> "&focus=%23" <> _encodeURIComponent f) affs
      ls <- case listenedYear ln of
        -- the play log: one row per play, with its time
        Just y -> do
          let url = api <> "listenedq/" <> y <> "?" <> query
          H.liftAff $ Console.log url
          r <- H.liftAff $ getUrl url
          let lje = (decodeJson =<< parseJson r) :: Either JsonDecodeError ListenedJ
          pure case lje of
            Right { lalbums: ls', lplayed: ps } ->
              zipWith (\a p -> (fst a) { albumShelf = snd a, albumPlayedAt = Just p }) ls' ps
            Left _ -> []
        Nothing -> do
          let url = api <> "albumsq/"
                  <>  _encodeURIComponent ln
                  <> (if contains ( Pattern "?" ) ln  then "&" else "?")
                  <> query
          H.liftAff $ Console.log url
          r <- H.liftAff $ getUrl url
          let lje = (decodeJson =<< parseJson r) :: Either JsonDecodeError AlbumsJ
          pure case lje of
            Right { lalbums: ls' } -> map (\a -> (fst a) { albumShelf = (snd a) } ) ls'
            Left _ -> []
      H.modify_ _ { loading = false, albumList = ls }

    -- "Listened" -> all years, "Listened 2026" -> 2026
    listenedYear :: String -> Maybe String
    listenedYear ln
      | ln == "Listened" = Just "all"
      | otherwise = case stripPrefix (Pattern "Listened ") ln of
          Just y | length y == 4 -> Just y
          _ -> Nothing
