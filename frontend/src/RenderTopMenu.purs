module RenderTopMenu (
  renderSide
, renderTop
, renderTools
, shownAlbums
) where

-- The app's frame (docs/menu-mockup.html, design A): a sidebar with everything that is
-- navigation, a slim top bar (‹ ›, title, search, player, ⋯), and — on list views only —
-- a toolbar for focus and sort. Menus open on click, so they work on a touch screen.

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import Data.Array (length, filter, null, elem, nub, mapMaybe, head, last, reverse, sort)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map as M
import Data.Set as Set
import Data.Tuple (Tuple(..), fst)
import Data.String (take, drop, toLower, contains, split, indexOf, trim) as S
import Data.String.Pattern (Pattern(..))

import Types (Album, SortOrder (..), State, AlbumList(..),  Action(..))

cls :: forall r i. String -> HP.IProp ( class :: String | r ) i
cls = HP.class_ <<< HH.ClassName

-- the albums of the list that the search box lets through
shownAlbums :: State -> Array Album
shownAlbums state =
  let q = S.toLower (S.trim state.search) in
  if q == "" then state.albumList
  else filter (\a -> S.contains (Pattern q) (S.toLower (a.albumTitle <> " " <> a.albumArtist))) state.albumList

inList :: State -> Boolean
inList state = case state.listName of
  AlbumList Nothing -> false
  _ -> true

-- the sidebar: collection lists, the play log, the Discogs lists, and the locations
-- folded by kind (Shelf, Cube, the rest). A drawer on a narrow screen.
renderSide :: forall m. State -> H.ComponentHTML Action () m
renderSide state =
  HH.div [ cls (if state.sideOpen then "side side-open" else "side") ] $
    [ HH.div [ cls "brand" ] [ HH.i_ [], HH.text "clutter" ] ]
    <> group "Library" library
    <> myTagsGroup
    <> group "Discogs Lists" lists
    <> fold "Genre folders" genreFolders
    <> listenedFold
    <> serverFold
    <> fold "Cubes" cubes
    <> fold "Shelves" shelves
    <> fold "Boxes and more" otherLocs
  where
  names = state.menu.params.mlistNames
  locs = state.menu.params.mlocNames
  isListened n = S.take 8 n == "Listened"
  listened = filter isListened names
  -- What is not the play log is told apart by the tags: a Discogs collection folder has a
  -- "folder.<name>" tag; a folder named after a medium (CD, CDR, Files, Streaming) also has a
  -- "format.<name>" tag, a genre folder (Piano, Pop, …) does not. The rest are the built-in
  -- collections and the Discogs lists.
  tags = state.menu.params.msts
  isFolder n = elem ("folder." <> S.toLower n) tags
  isMedium n = elem ("format." <> S.toLower n) tags
  builtIn n = elem n [ "Collection", "Inbox", "All", "Discogs", "Tidal", "Apple Music" ]
  -- the file albums on the server (Jellyfin): "Server" is all of them, "Server <name>" a
  -- storage location
  isServer n = n == "Server" || S.take 7 n == "Server "
  server = filter isServer names
  rest = filter (\n -> not (isListened n) && not (isServer n)) names
  library = filter (\n -> builtIn n || (isFolder n && isMedium n)) rest
  genreFolders = filter (\n -> isFolder n && not (isMedium n) && not (builtIn n)) rest
  lists = filter (\n -> not (isFolder n) && not (builtIn n)) rest
  firstWord n = fromMaybe "" (head (S.split (Pattern " ") n))
  shelves = filter (\n -> firstWord n == "Shelf") locs
  cubes = filter (\n -> firstWord n == "Cube") locs
  otherLocs = filter (\n -> not (elem (firstWord n) [ "Shelf", "Cube" ])) locs
  current = if inList state then state.menu.ln else ""
  item n =
    HH.button [ cls (if n == current then "nav-item nav-on" else "nav-item")
              , HE.onClick \_ -> ShowList (AlbumList (Just n)) ]
              [ HH.text n ]
  group title ns = if null ns then [] else [ HH.h4_ [ HH.text title ] ] <> map item ns
  -- the curated tags as views (album layer, slice 5): All, focused on the tag; a tag no
  -- album has yet (my.vocal) is listed with 0
  myTagsGroup =
    if null state.curated then [] else
      [ HH.h4_ [ HH.text "My tags" ] ]
      <> map (\c -> HH.button [ cls (if inList state && state.menu.ln == "All" && state.menu.ffs == [ Tuple c.ctTag true ] then "nav-item nav-on" else "nav-item")
                              , HE.onClick \_ -> ShowCurated c.ctTag
                              , HP.title (show c.ctCount <> " albums tagged " <> c.ctName) ]
                              [ HH.text c.ctName, HH.small [ cls "nav-count" ] [ HH.text (" " <> show c.ctCount) ] ])
             state.curated
  -- the play log, folded: a click on "Listened" shows this year's plays (the newest year in
  -- the log) and opens the fold; a click while a Listened list is on show closes or opens it,
  -- and closed it is blue, since the list on show is one of its own. Inside, every year and
  -- "All years". Not a <details>, whose own toggle would undo ours.
  onListened = elem current listened
  listenedShown = onListened && state.listenedOpen
  years = sort (filter (\n -> n /= "Listened") listened)
  thisYear = fromMaybe "Listened" (last years)
  listenedFold =
    if null listened then [] else
      [ HH.button [ cls (if listenedShown then "nav-fold nav-fold-open"
                         else if onListened then "nav-fold nav-fold-on"
                         else "nav-fold")
                  , HE.onClick \_ -> ListenedClick thisYear
                  , HP.title (if onListened then current else "Show " <> thisYear) ]
                  [ HH.text "Listened" ]
      ]
      <> (if listenedShown
            then [ HH.div [ cls "nav-folded" ]
                     ( map (\n -> itemAs (S.drop 9 n) n) (reverse years)
                       <> (if elem "Listened" listened then [ itemAs "All years" "Listened" ] else []) ) ]
            else [])
  itemAs label n =
    HH.button [ cls (if n == current then "nav-item nav-on" else "nav-item")
              , HE.onClick \_ -> ShowList (AlbumList (Just n)) ]
              [ HH.text label ]
  serverFold =
    if null server then [] else
      [ HH.details (if elem current server then [ HP.attr (HH.AttrName "open") "" ] else [])
          ([ HH.summary_ [ HH.text "On the server" ] ]
           <> map (\n -> itemAs (if n == "Server" then "Everything" else S.drop 7 n) n) server
           -- the review of the inventory's matches that were not linked for sure
           <> [ HH.button [ cls (if state.review /= Nothing then "nav-item nav-on" else "nav-item")
                          , HE.onClick \_ -> ShowReview
                          , HP.title "File albums that may be versions of albums in clutter: link or pass" ]
                          [ HH.text "Unsure matches" ] ])
      ]
  -- folded; open when the list on show is in it
  fold title ns =
    if null ns then [] else
      [ HH.details (if elem current ns then [ HP.attr (HH.AttrName "open") "" ] else [])
          ([ HH.summary_ [ HH.text title ] ] <> map item ns)
      ]

-- the top bar: ☰ (narrow screens), ‹ ›, what is on show, search in the list, the player, ⋯
renderTop :: forall m. State -> H.ComponentHTML Action () m
renderTop state =
  HH.div [ cls "top" ]
    [ HH.button [ cls "ic burger", HE.onClick \_ -> ToggleSide, HP.title "Lists and locations" ] [ HH.text "☰" ]
    , HH.button [ cls "ic ic-nav", HE.onClick \_ -> Back, HP.title "Back to the view before", HP.disabled (state.pos == 0) ]
                [ HH.text "‹" ]
    , HH.button [ cls "ic ic-nav", HE.onClick \_ -> Forward, HP.title "Forward again"
                , HP.disabled (state.pos >= length state.views - 1) ]
                [ HH.text "›" ]
    , HH.div [ cls "top-title" ] title
    , if inList state
        then HH.input [ cls "search", HP.placeholder "Search this list", HP.value state.search
                      , HE.onValueInput SetSearch ]
        else HH.text ""
    -- Bluesound Node transport; the last answer shows as the tooltip
    , HH.span [ cls "player" ]
        [ HH.button [ cls "ic", HE.onClick \_ -> NodeCtl "toggle", HP.title (state.nodeName <> ": pause / resume" <> nodeTip) ]
                    [ HH.text "⏯" ]
        , HH.button [ cls "ic", HE.onClick \_ -> NodeCtl "skip", HP.title (state.nodeName <> ": next track" <> nodeTip) ]
                    [ HH.text "⏭" ]
        , HH.button [ cls "ic", HE.onClick \_ -> NodeCtl "stop", HP.title (state.nodeName <> ": stop" <> nodeTip) ]
                    [ HH.text "⏹" ]
        , HH.small_ [ HH.text state.nodeName ]
        ]
    , HH.span [ cls "pop-anchor" ]
        [ HH.button [ cls "ic", HE.onClick \_ -> TogglePop "more", HP.title "More" ] [ HH.text "⋯" ]
        , HH.div [ cls (pop "more" <> " pop-right") ]
            [ HH.button [ cls "menuitem", HE.onClick \_ -> UpdateDiscogs, HP.disabled state.loading
                        , HP.title "Fetch the newest releases of the Discogs collection" ]
                        [ HH.text "Update from Discogs" ]
            , HH.button [ cls "menuitem", HE.onClick \_ -> UpdateTidal, HP.disabled state.loading
                        , HP.title "Take in new Tidal favourites and the Save for later list" ]
                        [ HH.text "Update from Tidal" ]
            , HH.button [ cls "menuitem", HE.onClick \_ -> UpdateServer, HP.disabled state.loading
                        , HP.title "Take in the albums Jellyfin has added on the file server (seconds)" ]
                        [ HH.text "Update from the server" ]
            , HH.button [ cls "menuitem", HE.onClick \_ -> UpdateServerFull, HP.disabled state.loading
                        , HP.title "Ask Jellyfin for every album: also finds those that left, moved or were retagged. Takes minutes." ]
                        [ HH.text "Full update from the server…" ]
            , HH.form [ cls "lookup", HE.onSubmit \ev -> MakeRequest ev ]
                [ HH.label_ [ HH.text "Open an album by id" ]
                , HH.input [ HP.value state.albumID, HP.placeholder "D6715388, T283130328", HE.onValueInput SetAlbumID ]
                , HH.button [ HP.type_ HP.ButtonSubmit, HP.disabled state.loading ] [ HH.text "Open" ]
                ]
            ]
        ]
    ]
  where
  pop p = if state.menuOpen == Just p then "pop pop-open" else "pop"
  nodeTip = case state.nodeMsg of
    Just m -> " — last: " <> m
    Nothing -> ""
  title
    | state.loading = [ HH.text (fromMaybe "Loading…" state.notice) ]
    | Just rv <- state.review =
        [ HH.text "Unsure matches"
        , HH.small_ [ HH.text (show (length rv.rvRows) <> " to review") ]
        ] <> case state.notice of
               Just m -> [ HH.small [ cls "notice" ] [ HH.text m ] ]
               Nothing -> []
    | inList state =
        let n = length (shownAlbums state)
            all = length state.albumList in
        [ HH.text state.menu.ln
        , HH.small_ [ HH.text (if n == all then show all <> " albums" else show n <> " of " <> show all) ]
        ] <> case state.notice of
               Just m -> [ HH.small [ cls "notice" ] [ HH.text m ] ]
               Nothing -> []
    | otherwise = case state.album of
        Just a -> [ HH.text a.albumTitle ] <> case state.notice of
          Just m -> [ HH.small [ cls "notice" ] [ HH.text m ] ]
          Nothing -> []
        Nothing -> []

-- the list toolbar: the focus as chips, the tag picker, a saved focus, sort and order
renderTools :: forall m. State -> H.ComponentHTML Action () m
renderTools state =
  HH.div [ cls "tools" ] $
    map chip ffs
    <> [ HH.span [ cls "pop-anchor" ]
           [ HH.button [ cls "pill", HE.onClick \_ -> TogglePop "focus" ] [ HH.text "+ Focus" ]
           , HH.div [ cls (pop "focus" <> " pop-focus") ] focusPicker
           ]
       , HH.button [ cls "pill", HE.onClick \_ -> SetFocus [ Tuple "played.never" true, Tuple "format.vinyl" true ]
                   , HP.title "Never played, on vinyl" ]
                   [ HH.text "★ Never-played vinyl" ]
       ]
    <> (if null ffs then [] else [ HH.button [ cls "pill", HE.onClick \_ -> SetFocus [] ] [ HH.text "Clear" ] ])
    <> [ HH.span [ cls "grow" ] []
       -- bulk Keep: select mode, where a click on a cover selects it
       , HH.button [ cls (if state.selecting then "pill av-on" else "pill"), HE.onClick \_ -> ToggleSelect
                   , HP.title "Select albums in this list, then mark them all: Keep / Not for me" ]
                   [ HH.text (if state.selecting then "Done" else "Select") ]
       , HH.span [ cls "pop-anchor" ]
           [ HH.button [ cls "pill", HE.onClick \_ -> TogglePop "sort" ] [ HH.text ("Sort: " <> state.menu.sortName <> " ▾") ]
           , HH.div [ cls (pop "sort" <> " pop-right") ]
               (map (\s -> HH.button [ cls "menuitem", HE.onClick \_ -> SetSort s ]
                                     [ HH.text ((if s == state.menu.sortName then "✓ " else "") <> s) ])
                    state.menu.params.msorts)
           ]
       , HH.button [ cls "pill", HE.onClick \_ -> ToggleSortOrder, HP.disabled state.loading
                   , HP.title "Ascending / descending" ]
                   [ HH.text (case state.menu.sso of
                                Asc -> "↑"
                                Desc -> "↓") ]
       ]
    <> (if not state.selecting then [] else
          [ HH.div [ cls "selbar" ]
              [ HH.span [ cls "sel-n" ] [ HH.text (show nsel <> " selected") ]
              , HH.button [ cls "pill", HE.onClick \_ -> SelectShown, HP.title "Select every album the list shows now (focus and search applied)" ]
                          [ HH.text ("All " <> show (length (shownAlbums state)) <> " shown") ]
              , HH.button [ cls "pill", HE.onClick \_ -> SelectNone, HP.disabled (nsel == 0) ] [ HH.text "None" ]
              , HH.span [ cls "sel-gap" ] []
              , HH.button [ cls "pill", HE.onClick \_ -> KeepSelected "yes", HP.disabled (nsel == 0 || state.loading)
                          , HP.title "Keep the selected albums: they are in the collection" ] [ HH.text "Keep" ]
              , HH.button [ cls "pill", HE.onClick \_ -> KeepSelected "no", HP.disabled (nsel == 0 || state.loading)
                          , HP.title "The selected albums are not for me: out of the collection and the Inbox" ] [ HH.text "Not for me" ]
              , HH.button [ cls "pill", HE.onClick \_ -> KeepSelected "unset", HP.disabled (nsel == 0 || state.loading)
                          , HP.title "Take the mark off the selected albums: back to the rule" ] [ HH.text "Take mark off" ]
              ]
          ])
  where
  ffs = state.menu.ffs
  nsel = Set.size state.selected
  pop p = if state.menuOpen == Just p then "pop pop-open" else "pop"
  -- a tag of the focus: a click on its name flips has / has not, × takes it out
  chip (Tuple t on) =
    HH.span [ cls (if on then "chipf" else "chipf chipf-not") ]
      [ HH.button [ HE.onClick \_ -> ToggleFocus t, HP.title (if on then "Albums with this tag — click: without it" else "Albums without this tag — click: take it out") ]
                  [ HH.text ((if on then "#" else "− #") <> t) ]
      , HH.button [ HE.onClick \_ -> SetFocus (filter (\x -> fst x /= t) ffs), HP.title "Take it out" ] [ HH.text "×" ]
      ]

  -- the tags by their prefix (genre., format., played., …); those without one are the
  -- Discogs styles. Click: has the tag, again: has not, again: off.
  tags = state.menu.params.msts
  tf = S.toLower (S.trim state.tagFilter)
  shown = if tf == "" then tags else filter (\t -> S.contains (Pattern tf) (S.toLower t)) tags
  prefix t = case S.indexOf (Pattern ".") t of
    Just i -> S.take i t
    Nothing -> ""
  prefixes = sort (nub (mapMaybe (\t -> let p = prefix t in if p == "" then Nothing else Just p) shown))
  sel :: M.Map String Boolean
  sel = M.fromFoldable ffs
  tagBtn t =
    HH.button [ cls (case M.lookup t sel of
                       Just true -> "tag tag-on"
                       Just false -> "tag tag-not"
                       Nothing -> "tag")
              , HE.onClick \_ -> ToggleFocus t ]
              [ HH.text t ]
  section title ts = if null ts then [] else [ HH.h5_ [ HH.text title ], HH.div_ (map tagBtn ts) ]
  focusPicker =
    [ HH.input [ cls "search", HP.placeholder "Filter the tags", HP.value state.tagFilter, HE.onValueInput SetTagFilter ] ]
    <> (prefixes >>= \p -> section p (filter (\t -> prefix t == p) shown))
    <> section "styles" (filter (\t -> prefix t == "") shown)
    <> [ HH.p [ cls "pop-note" ] [ HH.text "Click: has the tag · again: has not · again: off" ] ]
