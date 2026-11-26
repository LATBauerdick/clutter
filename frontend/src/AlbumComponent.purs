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
import Data.String (take, drop) as S
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.String.Common (replaceAll)
import Effect.Class.Console as Console
import Data.Array (foldMap, zipWith, snoc, take, index, updateAt)
import Halogen as H
import Halogen.Subscription as HS

import Effect.Aff.Class (class MonadAff)
import Web.Event.Event as Event

import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson)

import Types  (Album, AlbumJ, AlbumXJ, AlbumTJ, AlbumsJ, ListenedJ, ReviewJ, State, ParamsJ, AlbumList(..), Action(..), SortOrder(..), View(..))
import Data.Array (filter) as A
import Data.Set as Set
import RenderTopMenu (shownAlbums)
import GetStuff (confirmBox, getUrl, postStrings, getNow, _encodeURIComponent, initHistory, pushHistory, historyBack, historyForward, onPopState, isClamped)
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
    H.modify_ _ { listName = AlbumList Nothing, albumX = Nothing, albumT = Nothing, aboutOpen = false, aboutLong = false, version = "", verX = Nothing, verA = Nothing, loading = true }
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
  ShowVersion v -> do
    H.modify_ _ { version = v, verX = Nothing, verA = Nothing, albumT = Nothing, nodeMsg = Nothing }
    _ <- H.fork (loadTracks false)
    -- a file album: its own record and details, for the facts and for ▶
    when (S.take 1 v == "J") do
      api <- H.gets _.apiUrl
      ra <- H.liftAff $ getUrl (api <> "albumq/" <> v)
      case (decodeJson =<< parseJson ra) :: Either JsonDecodeError AlbumJ of
        Right { aid: _, album: va } -> H.modify_ \st -> if st.version == v then st { verA = Just va } else st
        Left _ -> pure unit
      rx <- H.liftAff $ getUrl (api <> "albumxq/" <> v)
      case (decodeJson =<< parseJson rx) :: Either JsonDecodeError AlbumXJ of
        Right x -> H.modify_ \st -> if st.version == v then st { verX = Just x } else st
        Left _ -> pure unit

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
        H.modify_ _ { views = vs, pos = j, menuOpen = Nothing, sideOpen = false, search = "", review = Nothing }
        case v of
          VReview -> loadReview
          VAlbum aid -> showAlbum aid
          VList alist ln ffs sn so -> do
            -- the entry brings its own sort; going back into the play log, keep the lists' sort
            H.modify_ _ { sortKept = if isListened ln && not (isListened st.menu.ln)
                                       then Just (Tuple st.menu.sortName st.menu.sso)
                                       else if isListened ln then st.sortKept else Nothing }
            H.modify_ _ { listName = alist, loading = true, menu { ln = ln, ffs = ffs, sortName = sn, sso = so } }
            updateAlbumList

  ShowListSort alist sn so -> do
    pushView
    H.modify_ _ { sortKept = Nothing }
    H.modify_ _ { listName = alist, loading = true, menu { ln = fromMaybe "Nothing" <<< unwrap $ alist, sso = so, sortName = sn } }
    updateAlbumList
  ShowList alist -> do
    pushView
    adjustSort (fromMaybe "Nothing" <<< unwrap $ alist)
    H.modify_ _ { listName = alist, loading = true, menu { ln = fromMaybe "Nothing" <<< unwrap $ alist } }
    updateAlbumList
  ShowFocus tag -> do
    pushView
    -- from the album view: all albums with this tag (a genre chip, a tag)
    let alist = AlbumList (Just "All")
    adjustSort "All"
    H.modify_ _ { listName = alist, loading = true, menu { ln = "All", ffs = [ Tuple tag true ] } }
    updateAlbumList
  AlbumPlayed aid -> do
    t <- titleOf aid
    ok <- H.liftEffect (confirmBox ("Log a play of " <> t <> " now?\n\nIt goes into the play log, an Obsidian note and a Day One entry."))
    when ok do
      H.modify_ _ { album = Nothing, albumX = Nothing, albumT = Nothing, aboutOpen = false, aboutLong = false, version = "", verX = Nothing, verA = Nothing, loading = true, listName = AlbumList Nothing }
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
  SetPlayFiles b -> H.modify_ _ { playFiles = b, nodeMsg = Nothing }
  NodePlayFiles aid mode -> do
    H.modify_ _ { nodeMsg = Just "Node: starting…" }
    api <- H.gets _.apiUrl
    r <- H.liftAff $ getUrl (api <> "nodeplayfiles/" <> aid <> "?mode=" <> mode)
    let msg = case (decodeJson =<< parseJson r) :: Either JsonDecodeError { reqInfo :: String } of
                Right { reqInfo: i } -> i
                Left _ -> r
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
    H.modify_ _ { menuOpen = Nothing, sideOpen = false, search = "", review = Nothing }
    H.modify_ _ { album = Nothing, albumX = Nothing, albumT = Nothing, aboutOpen = false, aboutLong = false, version = "", verX = Nothing, verA = Nothing, loading = true, listName = AlbumList Nothing }
    now <- H.liftAff getNow
    H.liftAff $ Console.logShow now
    api <- H.gets _.apiUrl
    _ <- H.liftAff $ getUrl (api <> "req?event=update")
    H.liftAff $ Console.logShow "Discogs Updated"
    let alist = AlbumList (Just "Discogs")
    adjustSort "Discogs"
    H.modify_ _ { listName = alist, loading = true, menu { ln = fromMaybe "Nothing" <<< unwrap $ alist } }
    updateAlbumList

  SetStreamRef s -> H.modify_ _ { streamRef = s }
  -- give the release the Tidal / Apple Music album named in the box (T<id>, A<id> or a
  -- share link; the backend parses it and appends the link to the collection log)
  LinkStream aid -> do
    api <- H.gets _.apiUrl
    ref <- H.gets _.streamRef
    let enc = replaceAll (Pattern " ") (Replacement "%20")
          <<< replaceAll (Pattern "+") (Replacement "%2B")
          <<< replaceAll (Pattern "=") (Replacement "%3D")
          <<< replaceAll (Pattern "&") (Replacement "%26")
          <<< replaceAll (Pattern "#") (Replacement "%23")
          <<< replaceAll (Pattern "?") (Replacement "%3F")
          <<< replaceAll (Pattern "%") (Replacement "%25")
    r <- H.liftAff $ getUrl (api <> "albumstream/" <> aid <> "?ref=" <> enc ref <> "&op=link")
    case (decodeJson =<< parseJson r) :: Either JsonDecodeError { reqInfo :: String, reqResult :: Int } of
      Right { reqInfo: i, reqResult: 1 } -> do
        showAlbum aid
        H.modify_ _ { notice = Just i, streamRef = "" }
      Right { reqInfo: i } -> H.modify_ _ { notice = Just i }
      Left _ -> H.modify_ _ { notice = Just "Link: no answer" }
  -- from a Tidal / Apple album's page: it is the stream of that release; then show the release
  LinkStreamTo aid s -> do
    api <- H.gets _.apiUrl
    r <- H.liftAff $ getUrl (api <> "albumstream/" <> aid <> "?ref=" <> s <> "&op=link")
    case (decodeJson =<< parseJson r) :: Either JsonDecodeError { reqInfo :: String, reqResult :: Int } of
      Right { reqInfo: i, reqResult: 1 } -> do
        pushView
        showAlbum aid
        H.modify_ _ { notice = Just i }
      Right { reqInfo: i } -> H.modify_ _ { notice = Just i }
      Left _ -> H.modify_ _ { notice = Just "Link: no answer" }
  UnlinkStream aid s -> do
    t <- titleOf aid
    ok <- H.liftEffect (confirmBox ("Take " <> streamName s <> " off " <> t <> "?\n\nThe link is taken out of clutter's collection log."))
    when ok do
      api <- H.gets _.apiUrl
      r <- H.liftAff $ getUrl (api <> "albumstream/" <> aid <> "?ref=" <> s <> "&op=unlink")
      case (decodeJson =<< parseJson r) :: Either JsonDecodeError { reqInfo :: String, reqResult :: Int } of
        Right { reqInfo: i, reqResult: 1 } -> do
          showAlbum aid
          H.modify_ _ { notice = Just i }
        Right { reqInfo: i } -> H.modify_ _ { notice = Just i }
        Left _ -> H.modify_ _ { notice = Just "Unlink: no answer" }
  -- the file album is a version of that album: link them (appended to the collection
  -- log), then show the album, which now stands for the files
  LinkFile aid version -> do
    api <- H.gets _.apiUrl
    r <- H.liftAff $ getUrl (api <> "albumlink/" <> aid <> "?version=" <> version <> "&op=link")
    case (decodeJson =<< parseJson r) :: Either JsonDecodeError { reqInfo :: String, reqResult :: Int } of
      Right { reqInfo: i, reqResult: 1 } -> do
        pushView
        showAlbum aid
        H.modify_ _ { notice = Just i }
      Right { reqInfo: i } -> H.modify_ _ { notice = Just i }
      Left _ -> H.modify_ _ { notice = Just "Link: no answer" }
  -- a wrong match: take the file album off this album (appended to the collection log)
  UnlinkFile aid version -> do
    t <- titleOf aid
    ok <- H.liftEffect (confirmBox ("Take these files off " <> t <> "?\n\nThey are no longer a version of this album; to put them back, link them again from the file album's page."))
    when ok do
      api <- H.gets _.apiUrl
      r <- H.liftAff $ getUrl (api <> "albumlink/" <> aid <> "?version=" <> version <> "&op=unlink")
      case (decodeJson =<< parseJson r) :: Either JsonDecodeError { reqInfo :: String, reqResult :: Int } of
        Right { reqInfo: i, reqResult: 1 } -> do
          showAlbum aid
          H.modify_ _ { notice = Just i }
        Right { reqInfo: i } -> H.modify_ _ { notice = Just i }
        Left _ -> H.modify_ _ { notice = Just "Unlink: no answer" }
  -- the keep mark: the backend appends it to the collection log and re-tags the album;
  -- the page stays, with the album as it now is
  KeepAlbum aid value -> do
    t <- titleOf aid
    ok <- H.liftEffect (confirmBox (case value of
                  "yes" -> "Keep " <> t <> "?\n\nIt goes into the collection."
                  "no" -> "Mark " <> t <> " as not for me?\n\nIt leaves the collection."
                  _ -> "Take the mark off " <> t <> "?\n\nThe collection rule decides again (record on a shelf, or played)."))
    when ok do
      api <- H.gets _.apiUrl
      r <- H.liftAff $ getUrl (api <> "albumkeep/" <> aid <> "?value=" <> value)
      case (decodeJson =<< parseJson r) :: Either JsonDecodeError { reqInfo :: String, reqResult :: Int } of
        Right { reqInfo: i, reqResult: 1 } -> do
          ra <- H.liftAff $ getUrl (api <> "albumq/" <> aid)
          case (decodeJson =<< parseJson ra) :: Either JsonDecodeError AlbumJ of
            Right { aid: _, album: a } -> H.modify_ _ { album = Just a, notice = Just i }
            Left _ -> H.modify_ _ { notice = Just i }
        Right { reqInfo: i } -> H.modify_ _ { notice = Just i }
        Left _ -> H.modify_ _ { notice = Just "Keep mark: no answer" }
  -- bulk Keep: select albums in a list, then mark them all at once
  ToggleSelect -> H.modify_ \st -> st { selecting = not st.selecting, selected = (Set.empty :: Set.Set String) }
  SelectAlbum aid -> H.modify_ \st -> st { selected = if Set.member aid st.selected then Set.delete aid st.selected else Set.insert aid st.selected }
  SelectShown -> H.modify_ \st -> st { selected = Set.fromFoldable (map _.albumID (shownAlbums st)) }
  SelectNone -> H.modify_ _ { selected = (Set.empty :: Set.Set String) }
  KeepSelected value -> do
    sel <- H.gets _.selected
    ok <- if Set.isEmpty sel then pure false else H.liftEffect (confirmBox (let n = show (Set.size sel) <> (if Set.size sel == 1 then " album" else " albums")
                 in case value of
                      "yes" -> "Keep these " <> n <> "?\n\nThey go into the collection."
                      "no" -> "Mark these " <> n <> " as not for me?\n\nThey leave the collection."
                      _ -> "Take the mark off these " <> n <> "?"))
    when ok do
      st <- H.get
      if Set.isEmpty st.selected then pure unit else do
        H.modify_ _ { loading = true, notice = Just ("Marking " <> show (Set.size st.selected) <> " albums…") }
        r <- H.liftAff $ postStrings (st.apiUrl <> "albumskeep?value=" <> value) (Set.toUnfoldable st.selected)
        let msg = case (decodeJson =<< parseJson r) :: Either JsonDecodeError { reqInfo :: String, reqResult :: Int } of
                    Right { reqInfo: i } -> i
                    Left _ -> "Keep mark: no answer"
        -- the list anew: in the Inbox the marked albums are gone from it
        updateAlbumList
        H.modify_ _ { notice = Just msg }
  ShowReview -> pushView *> loadReview
  -- from the review: the files are a version of that album; the row goes, the page stays
  ReviewLink aid version -> do
    api <- H.gets _.apiUrl
    r <- H.liftAff $ getUrl (api <> "albumlink/" <> aid <> "?version=" <> version <> "&op=link")
    case (decodeJson =<< parseJson r) :: Either JsonDecodeError { reqInfo :: String, reqResult :: Int } of
      Right { reqInfo: i, reqResult: 1 } -> reviewDone version i
      Right { reqInfo: i } -> H.modify_ _ { notice = Just i }
      Left _ -> H.modify_ _ { notice = Just "Link: no answer" }
  -- none of the albums offered: logged as a skip, not offered again
  ReviewSkip version -> do
    ok <- H.liftEffect (confirmBox ("None of these albums is this file album?\n\nIt will not be offered for review again (linking it by hand still works)."))
    when ok do
      api <- H.gets _.apiUrl
      r <- H.liftAff $ getUrl (api <> "albumskip/" <> version)
      case (decodeJson =<< parseJson r) :: Either JsonDecodeError { reqInfo :: String, reqResult :: Int } of
        Right { reqInfo: i, reqResult: 1 } -> reviewDone version i
        Right { reqInfo: i } -> H.modify_ _ { notice = Just i }
        Left _ -> H.modify_ _ { notice = Just "Skip: no answer" }
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
                    , listName = alist, menu { ln = "Tidal", ffs = [], sortName = "Added", sso = Asc }, sortKept = Nothing }
        updateAlbumList
      Right { reqInfo: i } -> H.modify_ _ { notice = Just i, loading = false }
      Left _ -> H.modify_ _ { notice = Just "Removing: no answer", loading = false }

  -- the backend asks Jellyfin for its new albums (or, full, for all of them: minutes) and
  -- takes the store in again, no restart. The view stays; a list on show is loaded anew.
  UpdateServer -> updateServer "jellyfin" "Updating from the server…"
  UpdateServerFull -> updateServer "jellyfinfull" "Full update from the server: this takes minutes…"

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
    H.modify_ _ { notice = Just msg, listName = alist, menu { ln = "Tidal", ffs = [], sortName = "Added", sso = Asc }, sortKept = Nothing }
    updateAlbumList

  where
    updateServer :: forall output' m'. MonadAff m' => String -> String -> H.HalogenM State Action () output' m' Unit
    updateServer event busy = do
      H.modify_ _ { menuOpen = Nothing, notice = Just busy }
      api <- H.gets _.apiUrl
      r <- H.liftAff $ getUrl (api <> "req?event=" <> event)
      let msg = case (decodeJson =<< parseJson r) :: Either JsonDecodeError { reqInfo :: String } of
                  Right { reqInfo: i } -> i
                  Left _ -> "Update from the server: no answer"
      ps <- H.liftAff $ getUrl (api <> "paramsq/all")
      case (decodeJson =<< parseJson ps) :: Either JsonDecodeError ParamsJ of
        Right { params: p } -> H.modify_ \st -> st { menu { params = p { muhq = st.menu.params.muhq } } }
        Left _ -> pure unit
      st <- H.get
      case st.listName, st.review of
        AlbumList (Just _), Nothing -> updateAlbumList
        _, _ -> pure unit
      H.modify_ _ { notice = Just msg }

    isListened :: String -> Boolean
    isListened n = S.take 8 n == "Listened"

    -- the play log opens in play order, whatever the other lists are sorted by; their sort
    -- is kept meanwhile and comes back with the next list that is not the play log.
    -- Call before menu.ln changes to the new list's name
    adjustSort :: forall output' m'. MonadAff m' => String -> H.HalogenM State Action () output' m' Unit
    adjustSort new = do
      st <- H.get
      case isListened st.menu.ln, isListened new of
        false, true -> H.modify_ _ { sortKept = Just (Tuple st.menu.sortName st.menu.sso), menu { sortName = "Default", sso = Asc } }
        true, false -> case st.sortKept of
          Just (Tuple sn so) -> H.modify_ _ { sortKept = Nothing, menu { sortName = sn, sso = so } }
          Nothing -> pure unit
        _, _ -> pure unit

    -- the review of the unsure matches: asked for anew each time it is shown
    loadReview :: forall output' m'. MonadAff m' => H.HalogenM State Action () output' m' Unit
    loadReview = do
      H.modify_ _ { album = Nothing, albumX = Nothing, albumT = Nothing, listName = AlbumList Nothing, loading = true, notice = Nothing }
      api <- H.gets _.apiUrl
      r <- H.liftAff $ getUrl (api <> "reviewq")
      case (decodeJson =<< parseJson r) :: Either JsonDecodeError ReviewJ of
        Right rv -> H.modify_ _ { review = Just rv, loading = false, notice = if rv.rvError == "" then Nothing else Just rv.rvError }
        Left _ -> H.modify_ _ { review = Just { rvRows: [], rvDone: 0, rvError: "" }, loading = false, notice = Just "Review: no answer from the server" }

    -- a row of the review is decided: it leaves the list
    reviewDone :: forall output' m'. MonadAff m' => String -> String -> H.HalogenM State Action () output' m' Unit
    reviewDone version msg =
      H.modify_ \st -> st { notice = Just msg
                          , review = map (\rv -> rv { rvRows = A.filter (\row -> row.rrKey /= version) rv.rvRows, rvDone = rv.rvDone + 1 }) st.review }

    currentView :: State -> Maybe View
    currentView st = case st.listName, st.album of
      _, _ | st.review /= Nothing -> Just VReview
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
      -- whatever comes next is not the review (ShowReview loads it again after this),
      -- and select mode ends with the view
      H.modify_ _ { review = Nothing, selecting = false, selected = (Set.empty :: Set.Set String) }

    -- the album's title for a question, quoted, if it is the one on show
    titleOf :: forall output' m'. MonadAff m' => String -> H.HalogenM State Action () output' m' String
    titleOf aid = do
      cur <- H.gets _.album
      pure $ case cur of
        Just c | c.albumID == aid -> "“" <> c.albumTitle <> "”"
        _ -> "this album"
    streamName s = (case S.take 1 s of
                      "T" -> "the Tidal album "
                      "A" -> "the Apple Music album "
                      "Q" -> "the Qobuz album "
                      _ -> "") <> S.drop 1 s

    showAlbum :: forall output' m'. MonadAff m' => String -> H.HalogenM State Action () output' m' Unit
    showAlbum aid = do
      H.modify_ _ { album = Nothing, albumX = Nothing, albumT = Nothing, aboutOpen = false, aboutLong = false, version = "", verX = Nothing, verA = Nothing, loading = true, listName = AlbumList Nothing, nodeMsg = Nothing }
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
    -- without /api/albumxq just leaves them out. Tracks are asked for at the same time
    -- (they may take seconds: fetched from Tidal, Discogs or Jellyfin the first time), and
    -- a Discogs release is checked with Discogs last (slice 18).
    loadDetail :: forall output' m'. MonadAff m' => Maybe Album -> H.HalogenM State Action () output' m' Unit
    loadDetail am = case am of
      Nothing -> pure unit
      Just a -> do
        _ <- H.fork (loadTracks false)
        api <- H.gets _.apiUrl
        r <- H.liftAff $ getUrl (api <> "albumxq/" <> a.albumID)
        cur <- H.gets _.album
        case (decodeJson =<< parseJson r) :: Either JsonDecodeError AlbumXJ, cur of
          Right x, Just c | x.xid == c.albumID -> H.modify_ _ { albumX = Just x }
          _, _ -> pure unit
        when (S.take 1 a.albumID == "D") (refreshFromDiscogs a.albumID)

    -- has Discogs changed the release since clutter read it? Then show it as it is now
    refreshFromDiscogs :: forall output' m'. MonadAff m' => String -> H.HalogenM State Action () output' m' Unit
    refreshFromDiscogs aid = do
      api <- H.gets _.apiUrl
      r <- H.liftAff $ getUrl (api <> "albumrefresh/" <> aid)
      cur <- H.gets _.album
      case (decodeJson =<< parseJson r) :: Either JsonDecodeError AlbumJ, cur of
        Right { aid: _, album: a' }, Just c | a'.albumID == c.albumID -> H.modify_ _ { album = Just a' }
        _, _ -> pure unit

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
          v <- H.gets _.version
          -- a file-album version has its own tracks; the Tidal version of a release is asked
          -- for through the release
          let fileV = S.take 1 v == "J"
              key = if fileV then v else a.albumID
              qs = (if refresh then [ "refresh=1" ] else []) <> (if v /= "" && not fileV then [ "source=" <> v ] else [])
          r <- H.liftAff $ getUrl (api <> "albumtq/" <> key <> (if qs == [] then "" else "?" <> joinWith "&" qs))
          cur <- H.gets _.album
          v' <- H.gets _.version
          case (decodeJson =<< parseJson r) :: Either JsonDecodeError AlbumTJ, cur of
            Right t, Just c | t.tjId == key && v' == v && (fileV || t.tjId == c.albumID) -> do
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
      H.modify_ _ { loading = false, albumList = ls, selected = (Set.empty :: Set.Set String) }

    -- "Listened" -> all years, "Listened 2026" -> 2026
    listenedYear :: String -> Maybe String
    listenedYear ln
      | ln == "Listened" = Just "all"
      | otherwise = case stripPrefix (Pattern "Listened ") ln of
          Just y | length y == 4 -> Just y
          _ -> Nothing
