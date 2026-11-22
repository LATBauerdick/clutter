module AlbumComponent (
  aComponent
  ) where

import Prelude
import Data.Newtype (unwrap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Either (Either(..))
import Data.Map as M
import Data.String (contains, stripPrefix, length, joinWith)
import Data.String (take) as S
import Data.String.Pattern (Pattern(..))
import Effect.Class.Console as Console
import Data.Array (foldMap, zipWith, snoc, take, index, updateAt)
import Halogen as H
import Halogen.Subscription as HS

import Effect.Aff.Class (class MonadAff)
import Web.Event.Event as Event

import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson)

import Types  (Album, AlbumJ, AlbumXJ, AlbumTJ, AlbumsJ, ListenedJ, State, ParamsJ, AlbumList(..), Action(..), SortOrder(..), View(..))
import GetStuff (getUrl, getNow, _encodeURIComponent, initHistory, pushHistory, historyBack, historyForward, onPopState, isClamped)
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
    H.modify_ _ { menuOpen = Nothing }
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
    pushView
    H.modify_ _ { listName = AlbumList Nothing, albumX = Nothing, albumT = Nothing, aboutOpen = false, aboutLong = false, tracksTidal = false, loading = true }
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

  ShowAlbum aid -> pushView *> showAlbum aid

  ToggleAbout -> H.modify_ \st -> st { aboutOpen = not st.aboutOpen }
  RefreshTracks -> loadTracks true
  TracksSource tidal -> do
    H.modify_ _ { tracksTidal = tidal }
    loadTracks false

  TogglePop p -> H.modify_ \st -> st { menuOpen = if st.menuOpen == Just p then Nothing else Just p }
  ClosePops -> H.modify_ _ { menuOpen = Nothing, sideOpen = false }
  ListenedClick year -> do
    st <- H.get
    let onListened = case st.listName of
          AlbumList (Just n) -> S.take 8 n == "Listened"
          _ -> false
    if onListened
      then H.modify_ _ { listenedOpen = not st.listenedOpen }
      else do
        H.modify_ _ { listenedOpen = true }
        handleAction (ShowList (AlbumList (Just year)))
  ToggleSide -> H.modify_ \st -> st { sideOpen = not st.sideOpen, menuOpen = Nothing }
  SetSearch s -> H.modify_ _ { search = s }
  SetTagFilter s -> H.modify_ _ { tagFilter = s }

  Back -> H.liftEffect historyBack
  Forward -> H.liftEffect historyForward
  GoTo j -> do
    st <- H.get
    -- keep the view we are leaving at its own entry, so that Forward finds it again
    let vs = case currentView st of
               Just v -> fromMaybe (snoc (take st.pos st.views) v) (updateAt st.pos v st.views)
               Nothing -> st.views
    case index vs j of
      Nothing -> pure unit -- an entry from before a reload: nothing to show
      Just v -> do
        H.modify_ _ { views = vs, pos = j, menuOpen = Nothing, sideOpen = false, search = "" }
        case v of
          VAlbum aid -> showAlbum aid
          VList alist ln ffs sn so -> do
            H.modify_ _ { listName = alist, loading = true, menu { ln = ln, ffs = ffs, sortName = sn, sso = so } }
            updateAlbumList

  ShowListSort alist sn so -> do
    pushView
    H.modify_ _ { listName = alist, loading = true, menu { ln = fromMaybe "Nothing" <<< unwrap $ alist, sso = so, sortName = sn } }
    updateAlbumList
  ShowList alist -> do
    pushView
    -- H.liftEffect $ Event.preventDefault event
    H.modify_ _ { listName = alist, loading = true, menu { ln = fromMaybe "Nothing" <<< unwrap $ alist } }
    updateAlbumList
  ShowFocus tag -> do
    pushView
    -- from the album view: all albums with this tag (a genre chip, a tag)
    let alist = AlbumList (Just "All")
    H.modify_ _ { listName = alist, loading = true, menu { ln = "All", ffs = [ Tuple tag true ] } }
    updateAlbumList
  AlbumPlayed aid -> do
    H.modify_ _ { album = Nothing, albumX = Nothing, albumT = Nothing, aboutOpen = false, aboutLong = false, tracksTidal = false, loading = true, listName = AlbumList Nothing }
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
    -- on start: also listen for the browser's Back and Forward
    { emitter, listener } <- H.liftEffect HS.create
    _ <- H.subscribe emitter
    H.liftEffect initHistory
    H.liftEffect $ onPopState (\j -> HS.notify listener (GoTo j))
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
    H.modify_ _ { menuOpen = Nothing, sideOpen = false, search = "" }
    H.modify_ _ { album = Nothing, albumX = Nothing, albumT = Nothing, aboutOpen = false, aboutLong = false, tracksTidal = false, loading = true, listName = AlbumList Nothing }
    now <- H.liftAff getNow
    H.liftAff $ Console.logShow now
    api <- H.gets _.apiUrl
    _ <- H.liftAff $ getUrl (api <> "req?event=update")
    H.liftAff $ Console.logShow "Discogs Updated"
    let alist = AlbumList (Just "Discogs")
    H.modify_ _ { listName = alist, loading = true, menu { ln = fromMaybe "Nothing" <<< unwrap $ alist } }
    updateAlbumList

  AskDrop ask -> H.modify_ _ { confirmDrop = ask }
  -- the backend marks the album as removed in the Tidal album store and forgets it; its
  -- plays stay in the log
  DropAlbum aid -> do
    H.modify_ _ { confirmDrop = false, loading = true, notice = Just "Removing…" }
    api <- H.gets _.apiUrl
    r <- H.liftAff $ getUrl (api <> "albumdrop/" <> aid)
    case (decodeJson =<< parseJson r) :: Either JsonDecodeError { reqInfo :: String, reqResult :: Int } of
      Right { reqInfo: i, reqResult: 1 } -> do
        let alist = AlbumList (Just "Tidal")
        H.modify_ _ { notice = Just i, album = Nothing, albumX = Nothing, albumT = Nothing
                    , listName = alist, menu { ln = "Tidal", ffs = [], sortName = "Added", sso = Asc } }
        updateAlbumList
      Right { reqInfo: i } -> H.modify_ _ { notice = Just i, loading = false }
      Left _ -> H.modify_ _ { notice = Just "Removing: no answer", loading = false }

  -- the backend runs update-json --tidal and takes the album store in again (no restart);
  -- then the Tidal list, newest first, and the menu's tags and lists anew
  UpdateTidal -> do
    pushView
    H.modify_ _ { album = Nothing, albumX = Nothing, albumT = Nothing, loading = true, notice = Just "Updating from Tidal…" }
    api <- H.gets _.apiUrl
    r <- H.liftAff $ getUrl (api <> "req?event=tidal")
    let msg = case (decodeJson =<< parseJson r) :: Either JsonDecodeError { reqInfo :: String } of
                Right { reqInfo: i } -> i
                Left _ -> "Update from Tidal: no answer"
    ps <- H.liftAff $ getUrl (api <> "paramsq/all")
    case (decodeJson =<< parseJson ps) :: Either JsonDecodeError ParamsJ of
      Right { params: p } -> H.modify_ \st -> st { menu { params = p { muhq = st.menu.params.muhq } } }
      Left _ -> pure unit
    let alist = AlbumList (Just "Tidal")
    H.modify_ _ { notice = Just msg, listName = alist, menu { ln = "Tidal", ffs = [], sortName = "Added", sso = Asc } }
    updateAlbumList

  where
    currentView :: State -> Maybe View
    currentView st = case st.listName, st.album of
      AlbumList Nothing, Just a -> Just (VAlbum a.albumID)
      AlbumList Nothing, Nothing -> Nothing
      l, _ -> Just (VList l st.menu.ln st.menu.ffs st.menu.sortName st.menu.sso)

    -- before showing something new: keep the view we are leaving at its history entry,
    -- drop what was ahead of it, and open a new entry for what comes
    pushView :: forall output' m'. MonadAff m' => H.HalogenM State Action () output' m' Unit
    pushView = do
      H.modify_ _ { menuOpen = Nothing, sideOpen = false, search = "", notice = Nothing, confirmDrop = false }
      st <- H.get
      case currentView st of
        Nothing -> pure unit
        Just v -> do
          H.modify_ _ { views = snoc (take st.pos st.views) v, pos = st.pos + 1 }
          H.liftEffect $ pushHistory (st.pos + 1)

    showAlbum :: forall output' m'. MonadAff m' => String -> H.HalogenM State Action () output' m' Unit
    showAlbum aid = do
      H.modify_ _ { album = Nothing, albumX = Nothing, albumT = Nothing, aboutOpen = false, aboutLong = false, tracksTidal = false, loading = true, listName = AlbumList Nothing, nodeMsg = Nothing }
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
        loadTracks false

    -- tracks, credits and review: from the backend's per-album cache, or fetched by it
    -- (refresh: fetched again)
    loadTracks :: forall output' m'. MonadAff m' => Boolean -> H.HalogenM State Action () output' m' Unit
    loadTracks refresh = do
      am <- H.gets _.album
      case am of
        Nothing -> pure unit
        Just a -> do
          H.modify_ _ { tracksLoading = true }
          api <- H.gets _.apiUrl
          tidal <- H.gets _.tracksTidal
          let qs = (if refresh then [ "refresh=1" ] else []) <> (if tidal then [ "source=tidal" ] else [])
          r <- H.liftAff $ getUrl (api <> "albumtq/" <> a.albumID <> (if qs == [] then "" else "?" <> joinWith "&" qs))
          cur <- H.gets _.album
          case (decodeJson =<< parseJson r) :: Either JsonDecodeError AlbumTJ, cur of
            Right t, Just c | t.tjId == c.albumID -> do
              -- the review is on the page, folded: is there more of it than the lines shown?
              H.modify_ _ { albumT = Just t, tracksLoading = false, aboutOpen = false }
              long <- H.liftEffect (isClamped ".av-about-text")
              H.modify_ _ { aboutLong = long }
            Right _, _ -> pure unit -- another album was opened meanwhile
            Left _, _ -> H.modify_ _ { tracksLoading = false }

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
