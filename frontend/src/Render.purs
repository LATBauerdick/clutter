module Render (
  render
  ) where

import Prelude
-- import CSS (CSS)
-- import CSS as CSS
-- import CSS.Background (backgroundColor)
-- import CSS.Color (Color, rgb)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Either (fromRight)
import Data.Tuple (Tuple(..), snd)
import Data.Array (range, length, replicate, zip, head, mapMaybe, elem, filter, concat, index, null, cons, any, nub)
import Data.Foldable (minimum, maximum)
import Data.Int (fromString)
import Halogen as H
import Halogen.HTML as HH
-- import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import Data.DateTime (DateTime)
import Data.Formatter.DateTime (formatDateTime)
import Data.String (take, drop, length, indexOf, contains, stripPrefix, joinWith, toLower, split) as S
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))

import Types (Album, AlbumRef, Track, State, AlbumList(..), Action(..), SortOrder(..), pLocList)
import RenderTopMenu (renderSide, renderTop, renderTools, shownAlbums)

render :: forall m0. State -> H.ComponentHTML Action () m0
render state =
  -- the frame (RenderTopMenu): sidebar, top bar, and on a list its toolbar
  HH.div [ HP.class_ (HH.ClassName "shell") ]
    [ renderSide state
    , HH.div [ HP.class_ (HH.ClassName "main") ] $
        [ renderTop state ]
        <> case state.listName of
             AlbumList Nothing -> [ albumView state.album state.now ]
             _ -> [ renderTools state, renderListView ]
    -- a click beside an open menu or the drawer closes it
    , if state.menuOpen /= Nothing || state.sideOpen
        then HH.div [ HP.class_ (HH.ClassName "backdrop"), HE.onClick \_ -> ClosePops ] []
        else HH.text ""
    ]

  where
  albumView :: forall m. Maybe Album -> DateTime -> H.ComponentHTML Action () m
  albumView am now = case am of
                    Just a -> HH.div_ [ renderAlbumView a now
                                      -- , renderPlayedButton a now
                                      ]
                    Nothing -> HH.div_ [ noAlbum ]

  -- renderPlayedButton :: forall m. Album -> DateTime -> H.ComponentHTML Action () m
  -- renderPlayedButton a _ =
  --   HH.div [ HP.class_ $ HH.ClassName "album-view" ]
  --          [ HH.button [ HE.onClick \_ -> AlbumPlayed (a.albumID) ]
  --                      [ HH.text " Album Played "]
  --          ]

  renderListView :: forall m. H.ComponentHTML Action () m
  renderListView =
    HH.div  [ HP.class_ $ HH.ClassName "albums" ]
            [ HH.div
              [ HP.class_ $ HH.ClassName "row"]
              ( map (\(Tuple i a) -> renderAlbumTN i a) $ zip idxs  as )
            ]
    where as = shownAlbums state -- what the search box lets through
          idxs = if pLocList state.listName
            then replicate (length as) 0
            else range 1 (length as)

  renderAlbumTN :: forall m. Int -> Album -> H.ComponentHTML Action () m
  renderAlbumTN idx' a =
    let idx = maybe idx' snd a.albumShelf in
    HH.div [ HP.class_ $ HH.ClassName "album-thumb" ]
      [ HH.div [ HP.class_ $ HH.ClassName "cover-container" ] (
          [ HH.div [ HP.class_ $ HH.ClassName "cover-img" ]
              [ HH.button [ HE.onClick \_ -> ShowAlbum (a.albumID) ]
                [ HH.img [ HP.src (if a.albumCover == "" then "/icons/no-cover.png" else a.albumCover)
                          , HP.alt "cover image"
                          , HP.class_ $ HH.ClassName "cover-image"
                          ]
                ]
              ]
          ] <> renderBadges idx a
        )
      -- , HH.span [ HP.class_ $ HH.ClassName "hovtext" ]
      --       [ HH.text a.albumTitle
      --       , HH.text a.albumArtist
      --       ]
      , renderHovAlbumInfo a
      , renderAlbumInfo a
      ]

  -- the artists' display names, without the Discogs "(n)" suffix, where the backend sends them
  artistOf :: Album -> String
  artistOf a = case filter (_ /= "") (map _.arName (fromMaybe [] a.albumArtists)) of
    [] -> a.albumArtist
    ns -> S.joinWith ", " ns

  renderHovAlbumInfo :: forall m. Album -> H.ComponentHTML Action () m
  renderHovAlbumInfo a =
    -- a card under the cover while the pointer is on it: title and artist in full (the
    -- lines under the cover cut them off), then year and format, the plays, the location
    let np = length a.albumListenedDates
        fmt = fromMaybe "" (head (S.split (Pattern ",") a.albumFormat))
        year = if a.albumReleased == "0" then "" else S.take 4 a.albumReleased
        plays = if np == 0 then (if a.albumPlays > 0 then "played before the log" else "not played yet")
                else "played " <> show np <> (if np == 1 then " time" else " times") <> " · last "
                     <> S.take 10 (fromMaybe "" (head a.albumListenedDates))
        loc = case a.albumShelf of
          Just (Tuple shelf i) -> shelf <> " · #" <> show i
          Nothing -> ""
        meta t = if t == "" then [] else [ HH.p [ HP.class_ $ HH.ClassName "hov-meta" ] [ HH.text t ] ]
    in
    HH.div [ HP.class_ $ HH.ClassName "hov-album-info" ] $
           [ HH.p [ HP.class_ $ HH.ClassName "hov-title" ] [ HH.text a.albumTitle ]
           , HH.p [ HP.class_ $ HH.ClassName "hov-artist" ] [ HH.text (artistOf a) ]
           ]
           <> meta (S.joinWith " · " (filter (_ /= "") [ year, fmt ]))
           <> meta plays
           <> meta loc

  renderAlbumInfo :: forall m. Album -> H.ComponentHTML Action () m
  renderAlbumInfo a =
    HH.div [ HP.class_ $ HH.ClassName "album-info" ] $
           [ HH.p [ HP.class_ $ HH.ClassName "album-title"]
                  [ HH.text a.albumTitle ]
           , HH.p [ HP.class_ $ HH.ClassName "album-artist"]
                  [ HH.text (artistOf a) ]
           ] <> case a.albumPlayedAt of
                  -- in the Listened views: when this play was, "2026-11-01 13:50"
                  Just p -> [ HH.p [ HP.class_ $ HH.ClassName "album-played" ]
                                   [ HH.text $ replaceAll (Pattern "T") (Replacement " ") (S.take 16 p) ] ]
                  Nothing -> []

  -- the album page, laid out after Roon's (docs/album-page-plan.md, design A, phase 0):
  -- one view for a Discogs release (D…) and for a stream without one (T…, A…)
  renderAlbumView :: forall m. Album -> DateTime -> H.ComponentHTML Action () m
  renderAlbumView a now =
    HH.div [ cls "album-view av" ]
      [ HH.div [ cls "av-hero" ]
        [ HH.a [ HP.href a.albumURL, cls "av-cover" ]
            [ HH.img [ HP.src (if a.albumCover == "" then "/icons/no-cover.png" else a.albumCover)
                     , HP.alt "cover image"
                     ]
            ]
        , HH.div [ cls "av-head" ]
          [ HH.p [ cls "av-date" ] [ HH.text (longDate (fromMaybe a.albumReleased (facts >>= _.afReleased))
                                              <> maybe "" (" · " <> _) (facts >>= _.afKind)) ]
          , HH.h1 [ cls "av-title" ] [ HH.text a.albumTitle ]
          , HH.p [ cls "av-artist" ] [ HH.text artistNames ]
          , whereView
          , actionsView
          ]
        ]
      , chipsView
      , HH.div [ cls "av-body" ]
        [ HH.div [ cls "av-main" ] (aboutView <> tracksView <> creditsView <> [ playsView ])
        , HH.div [ cls "av-side" ] [ factsView, readMoreView, openInView ]
        ]
      , HH.div_ stripsView
      ]
    where
    cls :: forall r i. String -> HP.IProp ( class :: String | r ) i
    cls = HP.class_ <<< HH.ClassName
    isDiscogs = S.take 1 a.albumID == "D"
    -- the details from /api/albumxq, once they are there and belong to this album
    detail = case state.albumX of
      Just x | x.xid == a.albumID -> Just x
      _ -> Nothing
    facts = _.xfacts <$> detail
    -- the Discogs styles, without those that are a genre chip already
    styles = filter (\st -> not (elem (S.toLower st) genres)) (nub (maybe [] _.afStyles facts))
    artistNames = artistOf a
    -- "2025-09-26" -> "26 September 2025"; a bare year stays
    longDate "0" = "" -- Discogs: no year
    longDate d = case S.length d == 10, fromString (S.take 2 (S.drop 5 d)), fromString (S.drop 8 d) of
      true, Just m, Just day | Just mn <- index months (m - 1) -> show day <> " " <> mn <> " " <> S.take 4 d
      _, _, _ -> d
    months = [ "January", "February", "March", "April", "May", "June", "July"
             , "August", "September", "October", "November", "December" ]

    -- where the physical album is: its location list and position there (from the details),
    -- or the CD box number from the Discogs Location field; a click shows that shelf
    whereIs = case detail >>= _.xloc of
      Just l -> Just l
      Nothing -> case a.albumLocation of
        Just loc | S.contains (Pattern "Box CDs") (S.take 7 loc) -> Just (Tuple "Box CDs" (fromMaybe 0 a.albumLocIdx))
        _ -> a.albumShelf
    whereView = case whereIs of
      Just (Tuple shelf i) ->
        HH.button [ cls "av-loc", HE.onClick \_ -> ShowListSort (AlbumList (Just shelf)) "Default" Asc
                  , HP.title ("Show " <> shelf) ]
                  [ HH.span_ [ HH.text "Location" ], HH.text (shelf <> " · #" <> show i) ]
      -- a physical album in no location list; said only once the details are there
      Nothing | isPhysical && detail /= Nothing ->
        HH.div [ cls "av-loc av-loc-none", HP.title "In none of the location lists (Shelf, Cube, Box, …)" ]
               [ HH.span_ [ HH.text "Location" ], HH.text "not filed" ]
      Nothing -> HH.text ""
    -- a Discogs release that is not a download or a stream
    isPhysical = isDiscogs && a.albumFormat /= "Streaming" && not (S.contains (Pattern "File") a.albumFormat)

    -- ▶ <player>: the Tidal version on the Bluesound Node (BluOS API, via the backend),
    -- with the backend's four modes; then Album Played
    actionsView =
      HH.div [ cls "av-actions" ] $
        ( case a.albumTidal of
            Just t | t /= "" ->
              [ HH.span [ cls "av-play" ]
                [ HH.button [ cls "av-btn av-primary", HE.onClick \_ -> NodePlay t "replace"
                            , HP.title ("Play the Tidal album on " <> state.nodeName <> ", replacing its queue") ]
                            [ HH.text ("▶ " <> state.nodeName) ]
                , modeBtn t "now" "Play it now, after that go on with the queue"
                , modeBtn t "next" "Queue it after the current album"
                , modeBtn t "last" "Add it to the end of the queue"
                ]
              ]
            _ -> [] )
        <> [ HH.button [ cls "av-btn", HE.onClick \_ -> AlbumPlayed a.albumID ] [ HH.text "✓ Album Played" ] ]
        <> case state.nodeMsg of
             Just m -> [ HH.span [ cls "av-msg" ] [ HH.text m ] ]
             Nothing -> []
    modeBtn t mode tip =
      HH.button [ cls "av-btn av-primary av-mode", HE.onClick \_ -> NodePlay t mode, HP.title tip ] [ HH.text mode ]

    -- genres as chips; a click shows that genre from all albums
    genres = mapMaybe (S.stripPrefix (Pattern "genre.")) a.albumTags
    -- the other tags, without those the facts already show (provider, rating, plays)
    -- and without the Discogs styles once those are chips
    otherTags = filter (\t -> not (any (\p -> S.take (S.length p) t == p) [ "genre.", "provider.", "rated.", "played.", "tidal." ])
                              && not (elem t (map S.toLower styles)) && not (elem t genres)) a.albumTags
    chipsView =
      HH.div [ cls "av-chips" ] $
        map (\g -> HH.button [ cls "av-chip", HE.onClick \_ -> ShowFocus ("genre." <> g), HP.title ("All albums: " <> g) ]
                             [ HH.text g ]) genres
        <> map (\s -> HH.button [ cls "av-chip av-style", HE.onClick \_ -> ShowFocus (S.toLower s), HP.title ("All albums: " <> s) ]
                                [ HH.text s ]) styles

    -- the facts column
    row k v = [ HH.span [ cls "av-k" ] [ HH.text k ], HH.span [ cls "av-v" ] v ]
    np = length a.albumListenedDates
    early = a.albumPlays > np -- the Discogs "Plays" mark also holds plays from before the log
    factsView =
      HH.div [ cls "av-facts" ] $ concat
        [ case facts >>= _.afLength of
            Just s -> row "Length" [ HH.text (show ((s + 30) / 60) <> " min"
                                              <> maybe "" (\n -> " · " <> show n <> " tracks") (facts >>= _.afTracks)) ]
            Nothing -> []
        , row "Format" [ HH.text a.albumFormat ]
        , case facts >>= _.afQuality of
            Just q -> row (if isDiscogs then "Tidal" else "Quality")
                          [ HH.text (q <> if maybe false _.afExplicit facts then " · explicit" else "") ]
            Nothing -> []
        , case facts >>= _.afLabel of
            Just l -> row "Label" [ HH.text l ]
            Nothing -> []
        , if a.albumRating > 0
            then row "Rating" [ HH.span [ cls "av-stars" ] [ HH.text (stars a.albumRating) ] ]
            else []
        , case a.albumAdded of
            Just d | d /= "" -> row "Added" [ HH.text (S.take 10 d) ]
            _ -> []
        , locationRow
        , row "Plays" [ HH.text playsText ]
        , if isDiscogs then [] else row "Tidal" [ HH.text collectionText ]
        , if otherTags == [] then [] else
            row "Tags" (map (\t -> HH.button [ cls "av-tag", HE.onClick \_ -> ShowFocus t, HP.title ("All albums: #" <> t) ]
                                             [ HH.text t ]) otherTags)
        ]
    stars n = S.joinWith "" (replicate (min 5 n) "★" <> replicate (5 - min 5 n) "☆")
    playsText
      | np == 0 = if early then "before the log began" else "not played yet"
      | otherwise = show np <> " · last " <> S.take 10 (fromMaybe "" (head a.albumListenedDates))
                    <> (if early then " (also before the log)" else "")
    collectionText = case a.albumTidal, a.albumFavoriteUntil of
      Nothing, _ -> "not on Tidal"
      Just _, _ | elem "tidal.later" a.albumTags && not (elem "tidal.collection" a.albumTags) ->
        "saved for later since " <> S.take 10 (fromMaybe "" a.albumAdded)
      Just _, _ | elem "tidal.collection" a.albumTags -> "in the collection since " <> S.take 10 (fromMaybe "" a.albumAdded)
      Just _, Just u -> "in the collection " <> S.take 10 (fromMaybe "" a.albumAdded) <> " – " <> S.take 10 u
      Just _, Nothing -> "not in the collection"
    -- a shelf or box is shown under the artist (whereView); here only a location that is a link
    locationRow = case a.albumLocation of
      Just loc | loc /= "" && whereIs == Nothing -> row "Location" [ HH.a [ HP.href loc ] [ HH.text loc ] ]
      _ -> []

    -- Wikipedia and AllMusic for the album and each artist (docs/wikidata-plan.md, phase 4);
    -- where Wikidata had nothing, a dimmed search link instead
    readMoreView =
      let artists = filter (\r -> r.arName /= "") (fromMaybe [] a.albumArtists)
          firstArtist = maybe a.albumArtist _.arName (head artists)
          link c tip url txt = HH.a [ HP.href url, cls c, HP.title tip, HP.target "_blank" ] [ HH.text txt ]
          found url txt = link "wiki-link" url url txt
          search url txt = link "wiki-search" "nothing in Wikidata: search" url txt
          wpSearch q = "https://en.wikipedia.org/w/index.php?search=" <> enc q
          amSearch kind q = "https://www.allmusic.com/search/" <> kind <> "/" <> enc q
          pair w am wq amq =
            [ maybe (search (wpSearch wq) "Wikipedia") (\u -> found u "Wikipedia") w
            , HH.text " · "
            , maybe (search amq "AllMusic") (\u -> found u "AllMusic") am
            ]
          albumQ = firstArtist <> " " <> a.albumTitle
          group name links = HH.p [ cls "wiki-links" ] ([ HH.span [ cls "av-who" ] [ HH.text (name <> ": ") ] ] <> links)
      in
      HH.div [ cls "av-links" ] $
        [ HH.h3_ [ HH.text "Read more" ]
        , group "Album" (pair a.albumWiki a.albumAllMusic albumQ (amSearch "albums" albumQ))
        ] <> map (\r -> group r.arName (pair r.arWiki r.arAllMusic r.arName (amSearch "artists" r.arName))) artists
    -- enough URL encoding for a search term
    enc = replaceAll (Pattern " ") (Replacement "+")
        <<< replaceAll (Pattern "/") (Replacement "%2F")
        <<< replaceAll (Pattern "?") (Replacement "%3F")
        <<< replaceAll (Pattern "#") (Replacement "%23")
        <<< replaceAll (Pattern "&") (Replacement "%26")
        <<< replaceAll (Pattern "+") (Replacement "%2B")
        <<< replaceAll (Pattern "%") (Replacement "%25")

    -- the album at Discogs and the streaming services
    openInView =
      let qry = enc (a.albumArtist <> " - " <> a.albumTitle)
          -- web pages open in a new tab; app links (tidal://, music://) go to the app
          ext url txt = [ HH.a [ HP.href url, HP.target "_blank" ] [ HH.text txt ] ]
          app url txt = [ HH.a [ HP.href url ] [ HH.text txt ] ]
          -- the Music app for a catalog album; a library album only has its web page
          apple am = if S.take 2 am == "l."
                       then ext ("https://music.apple.com/library/albums/" <> am) "Apple Music"
                       else app ("music://music.apple.com/us/album/" <> am) "Apple Music"
      in
      HH.div [ cls "av-links" ] $
        [ HH.h3_ [ HH.text "Open in" ] ]
        <> (if isDiscogs then ext a.albumURL "Discogs"
                         else ext ("https://www.discogs.com/search/?q=" <> qry <> "&type=all") "find on Discogs")
        <> (case a.albumTidal of
              Just t | t /= "" -> app ("tidal://album/" <> t) "Tidal app" <> ext ("https://listen.tidal.com/album/" <> t) "Tidal web"
              _ -> [])
        <> (case a.albumAMusic of
              Just am | am /= "" -> apple am
              _ -> [])
        <> (case a.albumJellyfin of
              Just j | j /= "" -> ext ("http://usrv.local:8096/web/#/details?id=" <> j) "Jellyfin"
              _ -> [])

    -- tracks, credits and review from /api/albumtq, once they are there and belong to this album
    tj = case state.albumT of
      Just t | t.tjId == a.albumID -> Just t
      _ -> Nothing
    tracks = tj >>= _.tjTracks

    -- the review: its first four lines, all of it after "more"; folded again with each album
    aboutView = case tracks >>= _.atReview of
      Just txt | txt /= "" ->
        [ HH.div [ cls "av-about" ]
            [ HH.div [ cls (if state.aboutOpen then "av-about-text" else "av-about-text av-clamp") ] [ HH.text txt ]
            -- only when the review is longer than the lines shown
            , if state.aboutLong || state.aboutOpen
                then HH.button [ cls "av-about-more", HE.onClick \_ -> ToggleAbout ]
                               [ HH.text (if state.aboutOpen then "less ▴" else "more ▾") ]
                else HH.text ""
            , HH.p [ cls "av-src" ] [ HH.text (fromMaybe "Review" (tracks >>= _.atReviewSource) <> ", via Tidal") ]
            ]
        ]
      _ -> []

    -- the track list, with a line per disc and per work; ↻ fetches it again
    tracksView =
      [ HH.div [ cls "av-tracks" ] $
          [ HH.div [ cls "av-sect-head" ] $
              [ HH.h2_ [ HH.text "Tracks" ] ]
              <> (case tracks of
                    Just t -> [ HH.span_ [ HH.text ("from " <> t.atSource <> " · " <> S.take 10 t.atFetched) ] ]
                    Nothing -> [])
              -- a physical release shows its own tracks; its Tidal version on request
              <> (case a.albumTidal of
                    Just t | isDiscogs && t /= "" ->
                      [ HH.button [ cls "av-tag", HE.onClick \_ -> TracksSource (not state.tracksTidal)
                                  , HP.disabled state.tracksLoading
                                  , HP.title (if state.tracksTidal then "Show the tracks of the Discogs release"
                                                                   else "Show the tracks of the Tidal version") ]
                                  [ HH.text (if state.tracksTidal then "Discogs release" else "Tidal version") ] ]
                    _ -> [])
              <> [ HH.button [ cls "av-tag", HE.onClick \_ -> RefreshTracks, HP.disabled state.tracksLoading
                             , HP.title "Fetch tracks, credits and review again" ] [ HH.text "↻" ] ]
          ] <> case tracks of
                 Just t -> trackRows t.atTracks
                 Nothing | state.tracksLoading -> [ HH.p [ cls "av-note" ] [ HH.text "Loading tracks…" ] ]
                         | otherwise -> [ HH.p [ cls "av-note" ] [ HH.text (maybe "No tracks." (\t -> "No tracks: " <> t.tjError) tj) ] ]
      ]
    trackRows :: Array Track -> Array (H.ComponentHTML Action () m)
    trackRows ts =
      let discs = nub (map _.tkDisc ts)
          multi = length discs > 1
          -- other artists than the album's on some track: show them (compilations)
          showArtists = any (\t -> not (null t.tkArtists) && S.joinWith ", " t.tkArtists /= artistNames) ts
                        && S.contains (Pattern "Various") a.albumArtist
          one (Tuple prev t) =
            (if multi && map _.tkDisc prev /= Just t.tkDisc
               then [ HH.div [ cls "av-disc" ] [ HH.text ("Disc " <> show t.tkDisc) ] ] else [])
            <> (case t.tkWork of
                  Just w | (prev >>= _.tkWork) /= Just w || map _.tkDisc prev /= Just t.tkDisc ->
                    [ HH.div [ cls "av-work" ] [ HH.text w ] ]
                  _ -> [])
            <> [ HH.div [ cls (if t.tkWork == Nothing then "av-trk" else "av-trk av-mov") ]
                 [ HH.span [ cls "av-trk-n" ] [ HH.text t.tkPos ]
                 , HH.span [ cls "av-trk-t" ] $
                     [ HH.text t.tkTitle ]
                     <> (if showArtists then [ HH.small_ [ HH.text (S.joinWith ", " t.tkArtists) ] ] else [])
                 , HH.span [ cls "av-trk-d" ] [ HH.text (maybe "" mmss t.tkSecs) ]
                 ]
               ]
      in concat (map one (zip (cons Nothing (map Just ts)) ts))
    mmss s = (if s >= 3600 then show (s / 3600) <> ":" <> two ((s `mod` 3600) / 60) else show (s / 60)) <> ":" <> two (s `mod` 60)
    two n = (if n < 10 then "0" else "") <> show n

    -- who did what, folded away
    creditsView = case tracks of
      Just t | not (null t.atCredits) ->
        [ HH.details [ cls "av-credits" ]
            [ HH.summary_ [ HH.text ("Credits (" <> show (length t.atCredits) <> ")") ]
            , HH.div [ cls "av-facts" ] (concat (map (\c -> row c.crRole [ HH.text (S.joinWith ", " c.crNames) ])
                                                    -- Discogs' catch-all "Other [...]" roles last
                                                    (filter (not <<< isOther) t.atCredits <> filter isOther t.atCredits)))
            ]
        ]
      _ -> []

    isOther c = S.take 5 c.crRole == "Other"

    -- the album strips: other versions, more by the artists, similar albums; all from the library
    stripsView = case _.xrelated <$> detail of
      Nothing -> []
      Just rel ->
        strip "Other versions" "in my library" rel.relVersions
        <> strip artistNames "in my library" rel.relByArtist
        <> strip "Similar albums" ("in my library · " <> S.drop 6 rel.relSimilarName <> " · longest not played first") rel.relSimilar
    strip title sub refs =
      if null refs then [] else
        [ HH.div [ cls "av-sect" ]
          [ HH.div [ cls "av-sect-head" ] [ HH.h2_ [ HH.text title ], HH.span_ [ HH.text sub ] ]
          , HH.div [ cls "av-strip" ] (map card refs)
          ]
        ]
    card :: AlbumRef -> H.ComponentHTML Action () m
    card r =
      let fmt = fromMaybe "" (head (S.split (Pattern ",") r.rFormat)) in
      HH.button [ cls "av-card", HE.onClick \_ -> ShowAlbum r.rId, HP.title (r.rArtist <> " – " <> r.rTitle) ] $
        [ HH.img [ HP.src (if r.rCover == "" then "/icons/no-cover.png" else r.rCover), HP.alt "" ]
        , HH.span [ cls "av-card-y" ] [ HH.text (S.joinWith " · " (filter (_ /= "") [ r.rYear, fmt ])) ]
        , HH.span [ cls "av-card-t" ] [ HH.text r.rTitle ]
        , HH.span [ cls "av-card-a" ] [ HH.text r.rArtist ]
        ] <> (if r.rNote == "" then [] else [ HH.span [ cls "av-card-y" ] [ HH.text r.rNote ] ])

    -- every play from the log, newest first, with a bar per year
    playYears = mapMaybe (fromString <<< S.take 4) a.albumListenedDates
    yearNow = fromMaybe 0 (fromString (fromRight "" (formatDateTime "YYYY" now)))
    playsView =
      HH.div [ cls "album-plays av-plays" ] $
        [ HH.h3_ [ HH.text "Play history" ] ] <>
        case a.albumListenedDates of
          [] -> [ HH.p_ [ HH.text (if early then "Played before the log began." else "Not played yet.") ] ]
          ds -> [ HH.p_ [ HH.text $ "Played " <> show np <> (if np == 1 then " time" else " times")
                                    <> (if early then ", and before the log began" else "") ]
                , yearsView
                , HH.ul_ (map (\d -> HH.li_ [ HH.text $ replaceAll (Pattern "T") (Replacement " ") (S.take 16 d) ]) ds)
                ]
    yearsView =
      let first = fromMaybe yearNow (minimum playYears)
          ys = range first (max first yearNow)
          count y = length (filter (_ == y) playYears)
          mx = max 1 (fromMaybe 1 (maximum (map count ys)))
          bar y =
            let n = count y
                h = if n == 0 then 2 else 8 + (26 * n) / mx
            in HH.div [ cls "av-year", HP.title (show y <> ": " <> show n) ]
                 [ HH.i [ cls (if n == 0 then "z" else "n"), HP.style ("height:" <> show h <> "px") ] []
                 , HH.text (S.drop 2 (show y))
                 ]
      in HH.div [ cls "av-years" ] (map bar ys)

--L.div_ [L.class_ "login-message"] $ do
-- L.p_ $ "Tidal Album <" <> show (albumID a) <> ">."
-- L.br_ []
-- let qry = T.replace " " "+" $ albumArtist a <> "+-+" <> albumTitle a
-- L.p_ $ do
--   L.toHtml ("Search \"" <> albumTitle a <> "\" by " <> albumArtist a <> " on ")
--   L.a_ [L.href_ $ "https://www.discogs.com/search/?q=" <> qry <> "&type=all"] "DISCOGS"
  -- allow :: forall r i. String -> HH.IProp ( allow :: String | r ) i
  -- allow = HH.prop (HH.PropName "allow")
  -- frameborder :: forall r i. String -> HH.IProp ( frameborder :: String | r ) i
  -- frameborder = HH.prop (HH.PropName "frameborder")

  noAlbum :: forall w i. HH.HTML w i
  noAlbum =
    HH.div
      [ HP.id "root" ]
      [
        HH.h1 [ ] [ HH.text "This is the Clutter App!" ]
      , HH.div
        [ HP.class_ $ HH.ClassName "login-message" ]
        [ HH.span_
          [ HH.h2 [ ] [ HH.text "...server is working..." ] ]
        ]
      ]

  renderBadges :: forall m. Int -> Album -> Array (H.ComponentHTML Action () m)
  renderBadges idx a =
    [ rbIndex idx
    , rbFormat a
    , rbTidal a
    , rbAMusic a
    , rbJellyfin a
    , rbRating a
    , rbPlays a
    , rbLocation a
    ]

  rbIndex :: forall m. Int -> H.ComponentHTML Action () m
  rbIndex idx =
    HH.div  [ HP.class_ $ HH.ClassName "idx"
            -- , HC.style do
            --     let idxColor :: Color
            --         idxColor = rgb 0xfd 0x7e 0x14
            --     backgroundColor idxColor
            ]
            [ HH.text $ " " <> show idx <> " " ]

  rbFormat :: forall m. Album -> H.ComponentHTML Action () m
  rbFormat a = do
    let f = a.albumFormat
        isInfixOf :: String -> String -> Boolean
        isInfixOf p s = S.indexOf (Pattern p) s /= Nothing
        c
          | f == "CD-R" = 'r'
          | "Vinyl" `isInfixOf` f && "Box Set" `isInfixOf` f = 'b'
          | "Vinyl, Vinly" `isInfixOf` f = 'b'
          | "Vinyl" `isInfixOf` f = 'v'
          | "SACD" `isInfixOf` f = 'g'
          | "CD" `isInfixOf` f && "Box Set" `isInfixOf` f = 'd'
          | "CD" `isInfixOf` f = 'c'
          | "DVD" `isInfixOf` f = 'e'
          | "Hybrid" `isInfixOf` f = 'c'
          | f == "Streaming" = 's'
          | f == "Files" = 'f'
          | f == "Tidal" = 't'
          | f == "AppleMusic" = 'a'
          | otherwise = 'x'

    HH.div [ HP.class_ $ HH.ClassName "cover-obackground" ]
      [ HH.a [ HP.href a.albumURL]
        case c of
          'v' ->
            [ HH.span [ HP.class_ $ HH.ClassName "fas fa-record-vinyl fa-sm" ] [] ]
          't' ->
            [ HH.img
                [ HP.src (state.menu.params.muhq <> "icons/tidal-is.png"), HP.alt "T"
                , HP.class_ $ HH.ClassName "cover-oimage"
                ]
            ]
          'a' ->
            [ HH.img
                [ HP.src (state.menu.params.muhq <> "icons/am-icon.png"), HP.alt "A"
                , HP.class_ $ HH.ClassName "cover-oimage"
                ]
            ]
          'b' ->
            [ HH.span [ HP.class_ $ HH.ClassName "far fa-clone fa-sm" ] [] ]
          'c' ->
            [ HH.img
                [ HP.src (state.menu.params.muhq <> "icons/cd-icon.png"), HP.alt "CD"
                , HP.class_ $ HH.ClassName "cover-oimage"
                ]
            ]
            -- [ HH.span [ HP.class_ $ HH.ClassName "fas fa-compact-disc fa-sm" ] [] ]
          'd' ->
            [ HH.span
                [ HP.class_ $ HH.ClassName "fa-stack fa-sm" ]
                [ HH.span [ HP.class_ $ HH.ClassName "fa fa-square-o fa-stack-1x" ] []
                , HH.span [ HP.class_ $ HH.ClassName "fa fa-compact-disc fa-stack-1x" ] []
                ]
            ]
          'e' ->
            [ HH.img
                [ HP.src (state.menu.params.muhq <> "icons/dvd-icon.png"), HP.alt "T"
                , HP.class_ $ HH.ClassName "cover-oimage"
                ]
            ]
          'f' ->
            [ HH.span [ HP.class_ $ HH.ClassName "far fa-file-audio fa-sm" ] [] ]
          'g' ->
            [ HH.img
                [ HP.src (state.menu.params.muhq <> "icons/sacd-icon.png"), HP.alt "SACD"
                , HP.class_ $ HH.ClassName "cover-oimage"
                ]
            ]
          'r' ->
            [ HH.img
                [ HP.src (state.menu.params.muhq <> "icons/cd-r-icon.png"), HP.alt "CDR"
                , HP.class_ $ HH.ClassName "cover-oimage"
                ]
            ]
          's' ->
            [ HH.span [ HP.class_ $ HH.ClassName "fas fa-wifi fa-sm" ] [] ]
          _ ->
            [ HH.text a.albumFormat ]
    ]

  rbTidal :: forall m. Album -> H.ComponentHTML Action () m
  rbTidal a = case a.albumTidal of
    Nothing -> HH.div_ []
    Just tid -> if a.albumFormat == "Tidal" then HH.div_ [] else -- don't show if just Tidal
      HH.div [ HP.class_ $ HH.ClassName "cover-obackground1" ]
      [ HH.a
        -- [ HP.href ("https://listen.tidal.com/album/" <> tid)]
        [ HP.href ("tidal://album/" <> tid)]
        [ HH.img
          [ HP.src (state.menu.params.muhq <> "icons/tidal-is.png")
          , HP.alt "T"
          , HP.class_ $ HH.ClassName "cover-oimage"
          ]
        ]
      ]

  rbAMusic :: forall m. Album -> H.ComponentHTML Action () m
  rbAMusic a = case a.albumAMusic of
    Nothing -> HH.div_ []
    Just amid -> if a.albumFormat == "AppleMusic" then HH.div_ [] else -- don't show if just Apple Music
      HH.div [ HP.class_ $ HH.ClassName "cover-obackground3" ]
      [ HH.a
        [ HP.href if S.take 2 amid == "l."
              then "https://music.apple.com/library/albums/" <> amid
              else "music://music.apple.com/us/album/" <> amid
              -- else "https://music.apple.com/us/album/" <> amid
        ]
        [ HH.img
          [ HP.src (state.menu.params.muhq <> "icons/am-icon.png")
          , HP.alt "A"
          , HP.class_ $ HH.ClassName "cover-oimage"
          ]
        ]
      ]

  rbJellyfin :: forall m. Album -> H.ComponentHTML Action () m
  rbJellyfin a = case a.albumJellyfin of
    Nothing -> HH.div_ []
    Just jid -> if a.albumFormat == "Jellyfin" then HH.div_ [] else -- don't show if just Jellyfin
      HH.div [ HP.class_ $ HH.ClassName "cover-obackground4"]
      [ HH.a
        [ HP.href ("http://usrv.local:8096/web/#/details?id=" <> jid)]
        [ HH.img
            [ HP.src (state.menu.params.muhq <> "icons/jellyfin-icon.png"), HP.alt "J"
            , HP.class_ $ HH.ClassName "cover-oimage"
            ]
        ]
      ]

  rbRating :: forall m. Album -> H.ComponentHTML Action () m
  rbRating a = case a.albumRating of
    1 ->  HH.div [ HP.class_ $ HH.ClassName "rat" ]
            [ HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                    , HP.style "color:lightgray" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                    , HP.style "color:lightgray" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                    , HP.style "color:lightgray" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                    , HP.style "color:lightgray" ] []
            ]
    2 ->  HH.div [ HP.class_ $ HH.ClassName "rat" ]
            [ HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                    , HP.style "color:lightgray" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                    , HP.style "color:lightgray" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                    , HP.style "color:lightgray" ] []
            ]
    3 ->  HH.div [ HP.class_ $ HH.ClassName "rat" ]
            [ HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                    , HP.style "color:lightgray" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                    , HP.style "color:lightgray" ] []
            ]
    4 ->  HH.div [ HP.class_ $ HH.ClassName "rat" ]
            [ HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star-o fa-sm"
                    , HP.style "color:lightgray" ] []
            ]
    5 ->  HH.div [ HP.class_ $ HH.ClassName "rat" ]
            [ HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            , HH.i [ HP.class_ $ HH.ClassName "fa fa-star fa-sm" ] []
            ]
    _ -> HH.div_ []

  rbPlays :: forall m. Album -> H.ComponentHTML Action () m
  rbPlays a =
    -- plays from clutter's play log; albumPlays is the Discogs field 7 mark, which also
    -- holds plays from before the log began
    let np = length a.albumListenedDates
        early = a.albumPlays > np in
    if np == 0 && early then
      HH.div [ HP.class_ $ HH.ClassName "plays"]
      [ HH.span_ [ HH.i [ HP.class_ $ HH.ClassName "far fa-check-circle", HP.style "color:lightgray" ] [] ]
      , HH.span [ HP.class_ $ HH.ClassName "hovtext" ] [ HH.text "Played before the log" ]
      ]
    else if np > 0 then
      HH.div [ HP.class_ $ HH.ClassName "plays"]
      [ case np of
      cnt | cnt == 1 -> HH.span_
            [ HH.i [ HP.class_ $ HH.ClassName "far fa-check-circle" ] [] ]
          | cnt == 2 -> HH.span_
            [ HH.i [ HP.class_ $ HH.ClassName "fa fa-thermometer-1" ] [] ]
          | cnt == 3 -> HH.span_
            [ HH.i [ HP.class_ $ HH.ClassName "fa fa-thermometer-2" , HP.style "color:orange" ] [] ]
          | cnt == 4 -> HH.span_
            [ HH.i [ HP.class_ $ HH.ClassName "fa fa-thermometer-3" , HP.style "color:red" ] [] ]
          | cnt >= 5 -> HH.span_
            [ HH.i
              [ HP.class_ $ HH.ClassName "fa fa-thermometer-4" , HP.style "color:red" ] [] ]
      _ ->  HH.div_ []
      , HH.span [ HP.class_ $ HH.ClassName "hovtext" ]
          [ HH.text $ "Played " <> show np <> " times, last " <> S.take 10 (fromMaybe "" (head a.albumListenedDates)) <> (if early then " (also before the log)" else "") ]
      ]
    else HH.div_ []

  rbLocation :: forall m. Album -> H.ComponentHTML Action () m
  rbLocation a = case a.albumLocation of
    Just loc ->
      let shelf = "Box CDs" in
      if S.contains (Pattern shelf) $ S.take 7 loc
        then
        -- treat CD Box special: this was a Discogs Release tagged with Loc C<id>
          HH.div [ HP.class_ $ HH.ClassName "cover-obackground2" ]
          [ HH.i [ HP.class_ $ HH.ClassName "fa fa-barcode" , HP.style "color:black" ] []
          , HH.button
            [ HP.class_ $ HH.ClassName "hovtext"
            , HE.onClick \_ -> ShowListSort (AlbumList (Just shelf)) "Default" Asc ]
            [ HH.text $ shelf <> " #" <> show (fromMaybe 0 a.albumLocIdx)
            ]
          ]
        else
        -- treat it as a URL...
          HH.div [ HP.class_ $ HH.ClassName "cover-obackground2" ]
            [ HH.i [ HP.class_ $ HH.ClassName "fa fa-barcode" , HP.style "color:red" ] []
            , HH.span
              [ HP.class_ $ HH.ClassName "hovtext" ] [ HH.text ""
              , HH.a
                  [ HP.class_ $ HH.ClassName "loclink"
                  , HP.href loc
                  ]
                  [ HH.text $ loc
                  ]
              ]
            ]
    Nothing ->
      case a.albumShelf of
        Just (Tuple shelf ipos) ->
          HH.div [ HP.class_ $ HH.ClassName "cover-obackground2" ]
          [ HH.i [ HP.class_ $ HH.ClassName "fa fa-barcode" , HP.style "color:black" ] []
          , HH.button
            [ HP.class_ $ HH.ClassName "hovtext"
            , HE.onClick \_ -> ShowListSort (AlbumList (Just shelf)) "Default" Asc ]
            [ HH.text $ "" <> shelf <> " #" <> show ipos
            ]
          ]
        Nothing ->
          case a.albumLocation of
            Just loc ->
              -- just treat loc as an URL
              HH.div [ HP.class_ $ HH.ClassName "cover-obackground2" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fa fa-barcode" , HP.style "color:red" ] []
                , HH.span
                  [ HP.class_ $ HH.ClassName "hovtext" ] [ HH.text "Location: "
                  , HH.a
                      [ HP.class_ $ HH.ClassName "loclink"
                      , HP.href loc
                      ]
                      [ HH.text $ loc <> " #" <> show 12
                      ]
                  ]
                ]
            Nothing -> HH.div [] []
