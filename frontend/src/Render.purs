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
import Data.Array (range, length, replicate, zip, head, mapMaybe, elem, filter, concat, index, null, cons, any, nub, find, sortWith, mapWithIndex)
import Control.Alt ((<|>))
import Data.Foldable (minimum, maximum, sum)
import Data.Traversable (traverse)
import Data.Set as Set
import Data.Int (fromString)
import Halogen as H
import Halogen.HTML as HH
-- import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import Data.DateTime (DateTime)
import Data.Formatter.DateTime (formatDateTime)
import Data.String (take, drop, length, indexOf, contains, stripPrefix, joinWith, toLower, split, trim) as S
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))

import Types (Album, AlbumRef, ReviewJ, Track, State, AlbumList(..), Action(..), SortOrder(..), pLocList)
import RenderTopMenu (renderSide, renderTop, renderTools, shownAlbums)
import Quality (bestVersion)

render :: forall m0. State -> H.ComponentHTML Action () m0
render state =
  -- the frame (RenderTopMenu): sidebar, top bar, and on a list its toolbar
  HH.div [ HP.class_ (HH.ClassName "shell") ]
    [ renderSide state
    , HH.div [ HP.class_ (HH.ClassName "main") ] $
        [ renderTop state ]
        <> case state.review, state.listName of
             Just rv, _ -> [ reviewView rv ]
             _, AlbumList Nothing -> [ albumView state.album state.now ]
             _, _ -> [ renderTools state, renderListView ]
    -- a click beside an open menu or the drawer closes it
    , if state.menuOpen /= Nothing || state.sideOpen
        then HH.div [ HP.class_ (HH.ClassName "backdrop"), HE.onClick \_ -> ClosePops ] []
        else HH.text ""
    ]

  where
  -- The review of the inventory's unsure matches (docs/jellyfin-plan.md): one block per file
  -- album -- what it is and where it lies -- and under it the albums it may be a version
  -- of, the inventory's choice first. Link makes the files a version of that album, "None
  -- of these" logs a skip; either way the block goes. The names open the albums.
  reviewView :: forall m. ReviewJ -> H.ComponentHTML Action () m
  reviewView rv =
    HH.div [ rc "review" ] $
      [ HH.p [ rc "rv-head" ]
          [ HH.text (if rv.rvError /= "" then rv.rvError
                     else if null rv.rvRows then "Nothing left to review."
                     else show (length rv.rvRows) <> " file albums on the server may be versions of albums in clutter. "
                          <> "Link the right one, or pass with “None of these”."
                          <> (if rv.rvDone > 0 then " " <> show rv.rvDone <> " done." else "")) ]
      ] <> map block rv.rvRows
    where
    rc :: forall r i. String -> HP.IProp ( class :: String | r ) i
    rc = HP.class_ <<< HH.ClassName
    block row =
      HH.div [ rc "rv-row" ] $
        [ HH.div [ rc "rv-file" ]
            [ HH.a [ HP.href "#", HE.onClick \_ -> ShowAlbum row.rrKey, HP.title "Open the file album" ]
                   [ HH.text (row.rrTitle <> " — " <> (if row.rrArtist == "" then "no album artist" else row.rrArtist)) ]
            , HH.small_ [ HH.text (" " <> S.joinWith " · " (filter (_ /= "")
                            [ row.rrYear, if row.rrMins == "" || row.rrMins == "0" then "" else row.rrMins <> " min", row.rrStorage ])) ]
            , if row.rrUrl == "" then HH.text "" else
                HH.a [ rc "rv-jf", HP.href row.rrUrl, HP.target "_blank", HP.title "Open in Jellyfin" ] [ HH.text "Jellyfin ↗" ]
            ]
        , HH.p [ rc "rv-why" ] [ HH.text (row.rrFolder <> (if row.rrFolder == "" then "" else " — ") <> row.rrWhy) ]
        ]
        <> map (cand row) row.rrCands
        <> [ HH.p [ rc "av-file" ]
               [ HH.button [ rc "pill rv-skip", HE.onClick \_ -> ReviewSkip row.rrKey
                           , HP.title "None of these is this album: do not offer it again" ]
                           [ HH.text "None of these" ] ] ]
    cand row c =
      HH.p [ rc "av-file" ]
        [ HH.button [ rc "pill", HE.onClick \_ -> ReviewLink c.cvKey row.rrKey
                    , HP.title "These files are a version of this album: link them" ]
                    [ HH.text "Link" ]
        , HH.a [ HP.href "#", HE.onClick \_ -> ShowAlbum c.cvKey, HP.title "Open this album" ]
               [ HH.text (c.cvTitle <> " — " <> c.cvArtist) ]
        , HH.small_ [ HH.text (" " <> c.cvFormat <> (if c.cvReleased == "" then "" else " · " <> S.take 4 c.cvReleased)) ]
        ]

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
    -- in select mode (bulk Keep) a click on the cover selects the album instead of opening it
    HH.div [ HP.class_ $ HH.ClassName (if state.selecting && Set.member a.albumID state.selected then "album-thumb album-sel" else "album-thumb") ]
      [ HH.div [ HP.class_ $ HH.ClassName "cover-container" ] (
          [ HH.div [ HP.class_ $ HH.ClassName "cover-img" ]
              [ HH.button [ HE.onClick \_ -> if state.selecting then SelectAlbum a.albumID else ShowAlbum (a.albumID) ]
                [ HH.img [ HP.src (if a.albumCover == "" then "/icons/no-cover.png" else a.albumCover)
                          , HP.alt "cover image"
                          , HP.class_ $ HH.ClassName "cover-image"
                          -- a list has thousands: fetch a cover when it comes near the window
                          , HP.attr (HH.AttrName "loading") "lazy"
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
            [ HH.img [ HP.src (if a.albumCover /= "" then a.albumCover else fromMaybe "/icons/no-cover.png" refCover)
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
        [ HH.div [ cls "av-main" ] (aboutView <> tracksView <> creditsView <> refDataView <> [ playsView ])
        , HH.div [ cls "av-side" ] [ versionsView, factsView, readMoreView, openInView, streamView, sameAsView, keepView, removeView ]
        ]
      , HH.div_ stripsView
      ]
    where
    cls :: forall r i. String -> HP.IProp ( class :: String | r ) i
    cls = HP.class_ <<< HH.ClassName
    isDiscogs = S.take 1 a.albumID == "D"
    jellyfin = state.menu.params.mjellyfin
    -- the album, or the Tidal album of this release, is off Tidal
    isGone = elem "tidal.gone" a.albumTags
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

    -- ▶ <player>: the album on the Bluesound Node (BluOS API, via the backend), with the
    -- backend's four modes; then Album Played. One button: it plays the Tidal version, or
    -- the album's files from the player's own library (the shares it has indexed). Where an
    -- album has both, a small chooser beside it says which; the choice stays while browsing.
    actionsView =
      HH.div [ cls "av-actions" ] $
        -- its Tidal album is off Tidal: say so; the files, if any, still play
        ( if hasTidal && isGone
            then
              [ HH.span [ cls "av-gone" ]
                  [ HH.text ("No longer on Tidal" <> maybe "" (\d -> " (noticed " <> S.take 10 d <> ")") (facts >>= _.afGone) <> " · ")
                  , HH.a [ HP.href ("https://listen.tidal.com/search?q=" <> enc (artistNames <> " " <> a.albumTitle)), HP.target "_blank" ]
                         [ HH.text "look for another edition" ]
                  ]
              ]
            else [] )
        <> ( if not canPlay then [] else
               [ HH.span [ cls "av-play" ]
                   [ HH.button [ cls "av-btn av-primary", HE.onClick \_ -> play "replace"
                               , HP.title ("Play " <> what <> " on " <> state.nodeName <> ", replacing its queue") ]
                               [ HH.text ("▶ " <> state.nodeName) ]
                   , modeBtn "now" [ queueIcon "M8 4h6 M2 8.5h12 M2 13h12 M2 1.5v2.5h4 M4.5 2.5L6 4 4.5 5.5" ] "Play it now, keeping the queue: what was queued after the song that was playing follows this album"
                   , modeBtn "next" [ queueIcon "M2 3h12 M8 8h6 M2 13h12 M2 5.5v2.5h4 M4.5 6.5L6 8 4.5 9.5" ] "Queue it next: after the song that is playing"
                   , modeBtn "last" [ queueIcon "M2 3h12 M2 7h12 M8 12h6 M2 9.5v2.5h4 M4.5 10.5L6 12 4.5 13.5" ] "Queue it last: at the end of the queue"
                   ]
               ]
               -- what ▶ plays, beside the buttons: the version chosen in Versions, or for the
               -- album itself its Tidal album, else its files
               <> [ HH.span [ cls "av-source-one", HP.title ("▶ plays " <> what <> " — choose another in Versions") ]
                            [ HH.text (if useFiles then "Files" <> maybe "" (\f -> if f.fvStorage == "" then "" else " · " <> f.fvStorage) playFile else "Tidal") ] ] )
        <> [ HH.button [ cls "av-btn", HE.onClick \_ -> AlbumPlayed a.albumID ] [ HH.text "✓ Album Played" ] ]
        <> case state.nodeMsg of
             Just m -> [ HH.span [ cls "av-msg" ] [ HH.text m ] ]
             Nothing -> []
    tidalId = fromMaybe "" a.albumTidal
    hasTidal = tidalId /= ""
    tidalPlays = hasTidal && not isGone
    -- a file album, or an album with file versions -- where the player has their folder in
    -- one of its shares (the backend asks the player; an older backend does not say, and
    -- then the files are offered and the player answers)
    hasFiles = (S.take 1 a.albumID == "J" || (case a.albumJellyfin of
      Just j -> j /= ""
      Nothing -> false)) && (case detail of
      Just d -> fromMaybe true d.xnode
      Nothing -> false)
    -- the version chosen in Versions decides what ▶ plays: the Tidal album, a file album
    -- (where the player has its folder), or for the album itself its Tidal album where
    -- there is one, else its files
    verIsFile = S.take 1 state.version == "J"
    verDetail = case state.verX of
      Just x | x.xid == state.version -> Just x
      _ -> Nothing
    verNode = verIsFile && maybe false (\x -> fromMaybe true x.xnode) verDetail
    -- the album itself: a release or a Tidal album plays Tidal where it can; a file album
    -- plays its own files (its Tidal album is a version of its own)
    -- A record's page stays on the record; its ▶ plays the version of the best quality
    -- (Slice 9: Quality.bestVersion, its Tidal album on a tie)
    recordBest = if isDiscogs then maybe "" (bestVersion a) detail else ""
    recordFile = S.take 1 recordBest == "J"
    useTidal = case state.version of
      "tidal" -> tidalPlays
      "" -> tidalPlays && not isFile && not recordFile
      _ -> false
    useFiles = case state.version of
      "tidal" -> false
      "" -> if recordFile then maybe false (\f -> fromMaybe true f.fvNode) playFile else not useTidal && hasFiles
      _ -> verNode
    canPlay = useTidal || useFiles
    -- the file album ▶ plays: the chosen one, a record's best, else the album's first
    playKey = if verIsFile then state.version else if recordFile then recordBest else "J" <> fromMaybe "" a.albumJellyfin
    playFile = find (\f -> f.fvKey == playKey) files
    what = if useFiles then "the files, from the player's own library," else "the Tidal album"
    play mode
      | verIsFile = NodePlayFiles state.version mode
      | useTidal = NodePlay tidalId mode
      | recordFile = NodePlayFiles recordBest mode
      | otherwise = NodePlayFiles a.albumID mode
    modeBtn mode label tip =
      HH.button [ cls "av-btn av-primary av-mode", HE.onClick \_ -> play mode, HP.title tip ] label
    -- a small line drawing of the queue: its lines, and an arrow to where the album goes
    queueIcon d =
      HH.elementNS svgNS (HH.ElemName "svg")
        [ HP.attr (HH.AttrName "viewBox") "0 0 16 16", HP.attr (HH.AttrName "width") "15", HP.attr (HH.AttrName "height") "15"
        , HP.attr (HH.AttrName "class") "av-qicon", HP.attr (HH.AttrName "aria-hidden") "true" ]
        [ HH.elementNS svgNS (HH.ElemName "path")
            [ HP.attr (HH.AttrName "d") d, HP.attr (HH.AttrName "fill") "none", HP.attr (HH.AttrName "stroke") "currentColor"
            , HP.attr (HH.AttrName "stroke-width") "1.6", HP.attr (HH.AttrName "stroke-linecap") "round", HP.attr (HH.AttrName "stroke-linejoin") "round" ]
            [] ]
    svgNS = HH.Namespace "http://www.w3.org/2000/svg"

    -- genres as chips; a click shows that genre from all albums
    genres = mapMaybe (S.stripPrefix (Pattern "genre.")) a.albumTags
    -- the other tags, without those the facts already show (provider, rating, plays)
    -- and without the Discogs styles once those are chips
    otherTags = filter (\t -> not (any (\p -> S.take (S.length p) t == p) [ "genre.", "provider.", "rated.", "played.", "tidal.", "length.", "collection.", "keep.", "my." ])
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
    -- no length on record: the sum of the track durations, if every track has one
    tracksSecs = case tj of
      Just t | Just at <- t.tjTracks, not (null at.atTracks) ->
        map sum (traverse _.tkSecs at.atTracks)
      _ -> Nothing
    -- the version's facts (Length, Format, Quality, Label, Added) follow the Versions
    -- choice; rating, plays, Tidal status and tags are the album's
    verFacts = _.xfacts <$> verDetail
    lengthRow s n = row "Length" [ HH.text (show ((s + 30) / 60) <> " min" <> maybe "" (\k -> " · " <> show k <> " tracks") n) ]
    factsView =
      HH.div [ cls "av-facts" ] $ concat
        [ case state.version of
            "" -> case (facts >>= _.afLength) <|> tracksSecs of
                    Just s -> lengthRow s (facts >>= _.afTracks)
                    Nothing -> []
            "tidal" -> maybe [] (\s -> lengthRow s (length <$> (tracks <#> _.atTracks))) tracksSecs
            _ -> case (verFacts >>= _.afLength) <|> tracksSecs of
                    Just s -> lengthRow s (length <$> (tracks <#> _.atTracks))
                    Nothing -> []
        , row "Format" [ HH.text (case state.version of
                                    "" | isFile -> "Files" <> maybe "" (" · " <> _) ownStorage
                                       | otherwise -> a.albumFormat
                                    "tidal" -> "Stream · Tidal"
                                    _ -> "Files" <> maybe "" (\f -> if f.fvStorage == "" then "" else " · " <> f.fvStorage) playFile) ]
        , case state.version of
            "" -> case facts >>= _.afQuality of
                    Just q -> row (if isDiscogs then "Tidal" else "Quality")
                                  [ HH.text (q <> if maybe false _.afExplicit facts then " · explicit" else "") ]
                    -- a file album: its files' format, once the tracks are there
                    Nothing | isFile, Just q <- tracks >>= _.atQuality -> row "Quality" [ HH.text q ]
                            | otherwise -> []
            "tidal" -> maybe [] (\q -> row "Quality" [ HH.text (q <> if maybe false _.afExplicit facts then " · explicit" else "") ]) (facts >>= _.afQuality)
            _ -> maybe [] (\q -> row "Quality" [ HH.text q ]) (tracks >>= _.atQuality)
        , if state.version /= "" then [] else case facts >>= _.afLabel of
            Just l -> row "Label" [ HH.text l ]
            Nothing -> []
        -- the album's rating (its rated.* tags: clutter's own once seeded from Discogs);
        -- a click on a star sets it, on the star that is set takes it off
        , row "Rating" (map (\i -> HH.button [ cls (if i <= rating then "av-star av-star-on" else "av-star")
                                             , HE.onClick \_ -> RateAlbum a.albumID (if i == rating then 0 else i)
                                             , HP.title (if i == rating then "Take the rating off" else "Rate " <> show i <> "★") ]
                                             [ HH.text (if i <= rating then "★" else "☆") ]) (range 1 5))
        -- the curated tags (my.*): × takes one off; the picker puts one on
        , row "My tags" (map (\t -> HH.span [ cls "av-mytag" ]
                                      [ HH.button [ cls "av-tag", HE.onClick \_ -> ShowCurated t, HP.title ("All albums tagged " <> curatedName t) ] [ HH.text (curatedName t) ]
                                      , HH.button [ cls "av-tag av-unlink", HE.onClick \_ -> TagAlbum a.albumID t false, HP.title ("Take " <> curatedName t <> " off") ] [ HH.text "×" ] ])
                             myTags
                         <> (if null tagChoices then [] else
                               [ HH.select [ cls "av-mytag-add", HE.onValueChange \v -> if v == "" then ClosePops else TagAlbum a.albumID v true ]
                                   ([ HH.option [ HP.value "", HP.selected true ] [ HH.text "+ tag" ] ]
                                    <> map (\c -> HH.option [ HP.value c.ctTag ] [ HH.text c.ctName ]) tagChoices) ]))
        , case (if verIsFile then state.verA >>= _.albumAdded else if state.version == "" then a.albumAdded else Nothing) of
            Just d | d /= "" -> row "Added" [ HH.text (S.take 10 d) ]
            _ -> []
        , locationRow
        , row "Plays" [ HH.text playsText ]
        , if isDiscogs then [] else row "Tidal" [ HH.text collectionText ]
        , if otherTags == [] then [] else
            row "Tags" (map (\t -> HH.button [ cls "av-tag", HE.onClick \_ -> ShowFocus t, HP.title ("All albums: #" <> t) ]
                                             [ HH.text t ]) otherTags)
        ]
    -- the rating from the rated.* tags ("rated.****": 4), 0 if none
    rating = fromMaybe 0 (maximum (mapMaybe (\t -> S.stripPrefix (Pattern "rated.*") t <#> \r -> 1 + S.length r) a.albumTags))
    myTags = filter (\t -> S.take 3 t == "my.") a.albumTags
    curatedName t = maybe (S.drop 3 t) _.ctName (find (\c -> c.ctTag == t) state.curated)
    tagChoices = filter (\c -> not (elem c.ctTag myTags)) state.curated
    playsText
      | np == 0 = if early then "before the log began" else "not played yet"
      | otherwise = show np <> " · last " <> S.take 10 (fromMaybe "" (head a.albumListenedDates))
                    <> (if early then " (also before the log)" else "")
    collectionText = case a.albumTidal, a.albumFavoriteUntil of
      Nothing, _ -> "not on Tidal"
      Just _, _ | isGone -> "gone from Tidal" <> (if elem "tidal.collection" a.albumTags then ", still among the favourites" else "")
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

    -- the file albums on the server that are versions of this album (docs/jellyfin-plan.md)
    files = fromMaybe [] (detail >>= _.xfiles)
    ownStorage = head (mapMaybe (S.stripPrefix (Pattern "storage.")) a.albumTags)

    -- Versions: the album itself (the record, the Tidal album or the files it was opened
    -- as), its Tidal album (a release), and its file albums on the server. A click shows
    -- that version: its facts, its tracks, and ▶ plays it. ↗ opens a version's own page;
    -- a file album also has its storage location (a click shows it), Jellyfin, and × for a
    -- wrong match. Shown where there is more than one.
    versionsView =
      let pick key label tip extra =
            HH.div [ cls (if state.version == key then "av-ver av-ver-on" else "av-ver") ] $
              [ HH.button [ cls "av-ver-pick", HE.onClick \_ -> ShowVersion key, HP.title tip ] [ HH.text label ] ] <> extra
          -- the album layer by hand (slice 3): every version of an album of two or more
          -- can be taken off (×), and one that clutter holds can lead the album
          -- (the server's list: not a stand-in release, nor the page of an Apple twin)
          albumKeys = fromMaybe [] (detail >>= _.xmembers)
          inAlbum k = elem k albumKeys
          leadKey = fromMaybe a.albumID (detail >>= _.xlead)
          leadSet = fromMaybe false (detail >>= _.xleadSet)
          -- the album's face in the lists (Slice 9; was "lead"): the version whose title,
          -- cover, year and date added the album's card shows. The rule picks it (record,
          -- Tidal, files, Apple Music, Qobuz); "use cover" on another version clutter holds
          -- chooses it instead, ↺ goes back to the rule.
          leadCtl k held
            | not (inAlbum k) = []
            | k == leadKey = [ HH.small [ cls "av-ver-note", HP.title (if leadSet then "The album's card shows this version's title and cover (chosen by hand)" else "The album's card shows this version's title and cover (by the rule: record, Tidal, files, Apple Music, Qobuz)") ]
                                [ HH.text "album cover" ] ]
                             <> (if leadSet then [ HH.button [ cls "av-tag", HE.onClick \_ -> LeadVersion a.albumID "", HP.title "Back to the rule: record, Tidal, files, Apple Music, Qobuz" ] [ HH.text "↺" ] ] else [])
            | held = [ HH.button [ cls "av-tag", HE.onClick \_ -> LeadVersion a.albumID k, HP.title "Show this version's title and cover on the album's card" ] [ HH.text "cover" ] ]
            | otherwise = []
          -- the version's quality, beside its name ("Hi-Res lossless", "FLAC 24/96")
          qualNote = maybe [] (\q -> [ HH.small [ cls "av-ver-note" ] [ HH.text q ] ])
          ownQuality
            | isTidalAlbum = facts >>= _.afQuality
            | isFile = detail >>= _.xquality
            | otherwise = Nothing
          -- a Tidal album that is no favourite is a version, but clutter does not hold it
          -- (xversions: key alone, no title), so it cannot lead
          tidalHeld = maybe true (\v -> v.cvTitle /= "") (find (\v -> v.cvKey == "T" <> tidalId) (fromMaybe [] (detail >>= _.xversions)))
          splitCtl k = if inAlbum k
            then [ HH.button [ cls "av-tag av-unlink", HE.onClick \_ -> SplitVersion a.albumID k, HP.title "Not this album: take this version off (an album of its own again)" ] [ HH.text "×" ] ]
            else []
          ownLabel
            | isDiscogs = S.trim (fromMaybe a.albumFormat (head (S.split (Pattern ",") a.albumFormat)))
                          <> maybe "" (\(Tuple shelf i) -> " · " <> shelf <> " #" <> show i) whereIs
            | isFile = "Files" <> maybe "" (" · " <> _) ownStorage
            | otherwise = "Tidal"
          tidalRow =
            -- a release's or a file album's Tidal album (a Tidal album is its own row)
            if not isTidalAlbum && hasTidal
              then [ pick "tidal" ("Tidal" <> if isGone then " · gone" else "") "The Tidal album: its tracks, quality and ▶"
                       (qualNote (facts >>= _.afQuality)
                        <> [ HH.a [ HP.href "#", HE.onClick \_ -> ShowAlbum ("T" <> tidalId), cls "av-ver-open", HP.title "Open the Tidal album's own page" ] [ HH.text "↗" ] ]
                        <> leadCtl ("T" <> tidalId) tidalHeld <> splitCtl ("T" <> tidalId)) ]
              else []
          fileRow f =
            pick f.fvKey ("Files" <> (if f.fvStorage == "" then "" else " · " <> f.fvStorage))
                 (f.fvTitle <> " — " <> f.fvArtist <> ": its tracks, quality and ▶")
                 ( qualNote f.fvQuality
                   <> [ HH.a [ HP.href "#", HE.onClick \_ -> ShowAlbum f.fvKey, cls "av-ver-open", HP.title "Open this file album's own page" ] [ HH.text "↗" ] ]
                   <> (if f.fvUrl == "" then [] else
                         [ HH.a [ HP.href f.fvUrl, HP.target "_blank", cls "av-ver-open", HP.title "Open in Jellyfin" ] [ HH.text "J" ] ])
                   <> (if f.fvStorage == "" then [] else
                         [ HH.button [ cls "av-tag", HE.onClick \_ -> ShowList (AlbumList (Just ("Server " <> f.fvStorage)))
                                     , HP.title ("All albums stored in " <> f.fvStorage) ] [ HH.text "≡" ] ])
                   <> leadCtl f.fvKey true
                   <> splitCtl f.fvKey
                   -- a different title, or linked to another release of this album: say so
                   <> (if f.fvTitle /= a.albumTitle then [ HH.small [ cls "av-ver-sub" ] [ HH.text f.fvTitle ] ] else [])
                   <> (case f.fvOwner of
                         Just o -> [ HH.small [ cls "av-ver-sub" ]
                                       [ HH.text "linked to the "
                                       , HH.a [ HP.href "#", HE.onClick \_ -> ShowAlbum o, HP.title "The release these files are linked to" ]
                                              [ HH.text (fromMaybe "other release" f.fvOwnerFormat) ] ] ]
                         Nothing -> []) )
          -- the album's other versions not shown above (album layer): other records, more
          -- editions of a stream, Apple Music, Qobuz; ↗ opens one that clutter holds
          shownKeys = [ a.albumID ] <> map _.fvKey files <> (if hasTidal then [ "T" <> tidalId ] else [])
          otherRow v =
            let kind = case S.take 1 v.cvKey of
                  "D" -> S.trim (fromMaybe v.cvFormat (head (S.split (Pattern ",") v.cvFormat)))
                  "T" -> "Tidal"
                  "A" -> "Apple Music"
                  "Q" -> "Qobuz"
                  "J" -> if v.cvFormat == "" then "Files" else v.cvFormat -- "Files · 1001"
                  _ -> "Version"
                held = v.cvTitle /= ""
            in
            HH.div [ cls "av-ver" ] $
              [ HH.span [ cls "av-ver-pick", HP.title (if held then v.cvTitle <> " — " <> v.cvArtist else v.cvKey <> ": not in clutter's library") ]
                        [ HH.text (kind <> (if held && v.cvReleased /= "" then " · " <> S.take 4 v.cvReleased else "")) ] ]
              <> (if held then [ HH.a [ HP.href "#", HE.onClick \_ -> ShowAlbum v.cvKey, cls "av-ver-open", HP.title "Open this version's own page" ] [ HH.text "↗" ] ] else [])
              <> leadCtl v.cvKey held <> splitCtl v.cvKey
          -- a version clutter does not hold (an Apple Music or Qobuz album only linked, not in
          -- the library) is not listed: nothing to show or play; it stays linked, under
          -- "Open in" and, with its ×, in the link block. A Tidal album is listed held or not
          -- (its tracks are fetched and it plays on the Node).
          others = filter (\v -> not (elem v.cvKey shownKeys) && (v.cvTitle /= "" || S.take 1 v.cvKey == "T"))
                          (fromMaybe [] (detail >>= _.xversions))
          ownRow = pick "" ownLabel "The album as it was opened" (qualNote ownQuality <> leadCtl a.albumID true <> splitCtl a.albumID)
          keyed = [ Tuple a.albumID ownRow ]
                  <> map (Tuple ("T" <> tidalId)) tidalRow
                  <> map (\f -> Tuple f.fvKey (fileRow f)) files
                  <> map (\v -> Tuple v.cvKey (otherRow v)) others
          kindRank k = case S.take 1 k of
            "D" -> 0
            "T" -> 1
            "J" -> 2
            "A" -> 3
            "Q" -> 4
            _ -> 5
          rows = map (\r -> r.row) $ sortWith (\r -> r.order) $
                   mapWithIndex (\i (Tuple k rw) -> { order: (if k == leadKey then 0 else 1000) + kindRank k * 100 + i, row: rw }) keyed
          -- a version that is not its album's lead: the album, with all its versions, one click away
          ofRow = case detail >>= _.xof of
            Just o -> [ HH.p [ cls "av-ver-of" ]
                          [ HH.text "Version of "
                          , HH.a [ HP.href "#", HE.onClick \_ -> ShowAlbum o.cvKey, HP.title (o.cvArtist <> " — " <> o.cvTitle <> ": the album and all its versions") ]
                                 [ HH.text (S.trim (fromMaybe o.cvFormat (head (S.split (Pattern ",") o.cvFormat))) <> " · " <> o.cvTitle) ] ] ]
            Nothing -> []
      in
      if length rows < 2 && null ofRow then HH.text "" else
        HH.div [ cls "av-files av-versions" ] ([ HH.h3_ [ HH.text "Versions" ] ] <> ofRow <> (if length rows < 2 then [] else rows))

    -- one box for what the album is linked to by hand (kept in clutter's log, nothing is
    -- written to Discogs): a release or a file album gets its Tidal / Apple Music / Qobuz
    -- album (T<id>, A<id>, Q<id> or a share link), and any album a Discogs release or master
    -- that describes it without being owned (D<id>, a discogs.com address;
    -- docs/album-layer-plan.md › Discogs references). Each can be taken off (×).
    streamView =
      let linked = fromMaybe [] (detail >>= _.xstreams)
          refs = fromMaybe [] (detail >>= _.xrefs)
          -- a stream can be linked only to a release or a file album (which may stand for itself)
          streams = isDiscogs || S.take 1 a.albumID == "J"
          label s = (case S.take 1 s of
                       "T" -> "Tidal "
                       "A" -> "Apple Music "
                       "Q" -> "Qobuz "
                       _ -> "") <> S.drop 1 s
          one s =
            HH.p [ cls "av-file" ]
              [ HH.text (label s)
              , HH.button [ cls "av-tag av-unlink", HE.onClick \_ -> UnlinkStream a.albumID s
                          , HP.title "Not this album's stream: take it off" ] [ HH.text "×" ] ]
          refLabel ref = (if S.take 7 ref == "master/" then "Discogs master " <> S.drop 7 ref else "Discogs release " <> S.drop 8 ref)
          oneRef ref =
            HH.p [ cls "av-file" ]
              [ HH.a [ HP.href ("https://www.discogs.com/" <> ref), HP.target "_blank", HP.title "Open on discogs.com" ] [ HH.text (refLabel ref) ]
              , HH.button [ cls "av-tag av-unlink", HE.onClick \_ -> DropRef a.albumID ref
                          , HP.title "Does not describe this album: take it off" ] [ HH.text "×" ] ]
          box =
            HH.div [ cls "av-streambox" ]
              [ HH.input [ HP.value state.streamRef
                         , HP.placeholder (if streams then "T<id>, A<id>, Q<id>, [r<id>] or a link" else "[r<id>], D<id> or a discogs.com link")
                         , HE.onValueInput SetStreamRef
                         , HP.title ((if streams then "A Tidal, Apple Music or Qobuz album (another edition takes the place of the one it has), or " else "")
                                     <> "a Discogs release or master that describes this album: credits, label, year. Nothing is added to the Discogs collection.") ]
              , HH.button [ cls "pill", HP.type_ HP.ButtonButton, HE.onClick \_ -> LinkStream a.albumID, HP.disabled (S.trim state.streamRef == "") ]
                          [ HH.text "Link" ]
              ]
      in
      if detail == Nothing then HH.text "" else
        HH.div [ cls "av-files" ] $
          [ HH.h3_ [ HH.text (if streams then "Link a stream or Discogs release" else "Link a Discogs release") ] ]
          <> (if streams then map one linked else []) <> map oneRef refs <> [ box ]

    -- on a file album's page: the albums it may be a version of (same or similar title);
    -- a click links the files to that album and shows it; the name opens it first
    -- since slice 3 of the album layer: on any album's page (its row), the albums it may be
    -- the same as, and a search for any other; Join makes the two one album (nothing is
    -- replaced: a version that does not belong is taken off with × in Versions)
    sameAsView =
      let cands = fromMaybe [] (detail >>= _.xcandidates)
          joinBtn c =
            HH.button [ cls "pill", HE.onClick \_ -> JoinAlbum c.cvKey a.albumID
                      , HP.title "The same album: join them (every version of both)" ] [ HH.text "Join" ]
          name c =
            [ HH.a [ HP.href "#", HE.onClick \_ -> ShowAlbum c.cvKey, HP.title "Open this album" ]
                   [ HH.text (c.cvTitle <> " — " <> c.cvArtist) ]
            , HH.small_ [ HH.text (" " <> c.cvFormat <> (if c.cvReleased == "" then "" else " · " <> S.take 4 c.cvReleased)
                                   <> (case c.cvHas of
                                         Just h | h /= "" -> " · has " <> h <> " (it stays: a second edition)"
                                         _ -> "")) ] ]
          one c =
            HH.p [ cls "av-file" ] $
              [ joinBtn c ] <> name c <>
              [ HH.button [ cls "av-tag av-unlink", HE.onClick \_ -> NotSame a.albumID c.cvKey true
                          , HP.title "Not the same album: don't suggest it again" ] [ HH.text "×" ]
              ]
          hit c = HH.p [ cls "av-file" ] ([ joinBtn c ] <> name c)
          isRow = case detail of
            Just x -> x.xof == Nothing
            Nothing -> false
          box =
            HH.div [ cls "av-streambox" ]
              [ HH.input [ HP.value state.findQ, HP.placeholder "Find another album: title, artist", HE.onValueInput SetFindQ
                         , HP.title "Words of the title and the artist (beginnings are enough)" ]
              , HH.button [ cls "pill", HP.type_ HP.ButtonButton, HE.onClick \_ -> FindAlbums a.albumID, HP.disabled (S.trim state.findQ == "") ]
                          [ HH.text "Find" ]
              ]
          -- those marked × , small, each with ↺ to suggest it again
          notSame = fromMaybe [] (detail >>= _.xnotsame)
          gone c =
            HH.p [ cls "av-file av-notsame" ]
              [ HH.text ("not " <> c.cvTitle <> " — " <> c.cvArtist <> " ")
              , HH.button [ cls "av-tag", HE.onClick \_ -> NotSame a.albumID c.cvKey false
                          , HP.title "Suggest it again" ] [ HH.text "↺" ] ]
      in
      if not isRow then HH.text "" else
        HH.div [ cls "av-files" ] ([ HH.h3_ [ HH.text "Same album as…" ] ] <> map one cands <> map gone notSame <> [ box ] <> map hit state.found)

    -- in the collection or not, why, and the keep mark (docs/jellyfin-plan.md, "Membership"):
    -- a record on a shelf and anything played is in; "Keep" puts an album in, "Not for me"
    -- takes it out; a click on the mark that is set takes it off again
    keepView =
      let has t = elem t a.albumTags
          kept = has "keep.yes"
          refused = has "keep.no"
          played = not (has "played.never")
          status
            | refused = "Not in the collection — marked “not for me”"
            | has "collection.in" =
                "In the collection — " <> (if kept then "kept" else if isPhysical then "on the shelf" else if played then "played" else "a record")
                <> (if kept && played then ", played" else "")
            | has "collection.inbox" = "In the inbox — not judged yet"
            | otherwise = ""
          btn on label value tip =
            HH.button [ cls (if on then "pill av-on" else "pill"), HE.onClick \_ -> KeepAlbum a.albumID value, HP.title tip ]
                      [ HH.text label ]
      in
      if status == "" then HH.text "" else
        HH.div [ cls "av-keep" ]
          [ HH.h3_ [ HH.text "Collection" ]
          , HH.p_ [ HH.text status ]
          -- an album the rule has in already (on the shelf, played) needs no Keep
          , if kept then btn true "✓ Kept" "unset" "Take the mark off: back to the rule (a record on a shelf, or played)"
            else if has "collection.in" then HH.text ""
            else btn false "Keep" "yes" "Put this album in the collection"
          , if refused then btn true "✓ Not for me" "unset" "Take the mark off: back to the rule (a record on a shelf, or played)"
                       else btn false "Not for me" "no" "Leave this album out of the collection, whatever the rule says"
          ]

    -- a stream album can be removed from clutter: asked once more, then marked in the store
    removeView =
      if S.take 1 a.albumID /= "T" then HH.text "" else
        HH.div [ cls "av-remove" ] $
          if state.confirmDrop
            then [ HH.span_ [ HH.text "Remove this album from clutter? Its plays stay in the log." ]
                 , HH.button [ cls "av-tag av-danger", HE.onClick \_ -> DropAlbum a.albumID ] [ HH.text "Remove" ]
                 , HH.button [ cls "av-tag", HE.onClick \_ -> AskDrop false ] [ HH.text "Keep" ]
                 ]
            else [ HH.button [ cls "av-remove-ask", HE.onClick \_ -> AskDrop true
                             , HP.title "Take this stream album out of clutter; saving it again on Tidal brings it back" ]
                             [ HH.text "Remove from clutter…" ] ]

    -- no cover of its own: the image of its first Discogs reference that has one, as soon as
    -- the references are there (the server shows it on the list cards from then on; a cover
    -- of its own, a Jellyfin image found later, always wins)
    refCover = case state.refsX of
      Just (Tuple rid rs) | rid == a.albumID -> head (mapMaybe (\r -> r.rfData >>= _.drCover) rs)
      _ -> Nothing

    -- the album at Discogs and the streaming services
    openInView =
      let qry = enc (a.albumArtist <> " - " <> a.albumTitle)
          -- a small mark of the service in front of each link
          icon f = HH.img [ HP.src (state.menu.params.muhq <> "icons/" <> f), HP.alt "", cls "av-link-icon" ]
          -- web pages open in a new tab; app links (tidal://, music://) go to the app
          ext f url txt = [ HH.a [ HP.href url, HP.target "_blank" ] [ icon f, HH.text txt ] ]
          app f url txt = [ HH.a [ HP.href url ] [ icon f, HH.text txt ] ]
          -- the Music app for a catalog album; a library album only has its web page
          apple am = if S.take 2 am == "l."
                       then ext "am-is.png" ("https://music.apple.com/library/albums/" <> am) "Apple Music"
                       else app "am-is.png" ("music://music.apple.com/us/album/" <> am) "Apple Music"
      in
      HH.div [ cls "av-links" ] $
        [ HH.h3_ [ HH.text "Open in" ] ]
        <> (if isDiscogs then ext "vinyl-icon.png" a.albumURL "Discogs"
                         else ext "vinyl-icon.png" ("https://www.discogs.com/search/?q=" <> qry <> "&type=all") "find on Discogs")
        <> (case a.albumTidal of
              Just t | t /= "" -> app "tidal-icon.png" ("tidal://album/" <> t) "Tidal app" <> ext "tidal-icon.png" ("https://listen.tidal.com/album/" <> t) "Tidal web"
              -- no Tidal album yet: Tidal's own search, in a new tab; its album address goes
              -- into the link box below
              _ | not isTidalAlbum -> ext "tidal-icon.png" ("https://listen.tidal.com/search?q=" <> enc (artistNames <> " " <> a.albumTitle)) "find on Tidal"
              _ -> [])
        <> (case a.albumAMusic of
              Just am | am /= "" -> apple am
              _ -> [])
        <> (case a.albumJellyfin of
              Just j | j /= "" && jellyfin /= "" -> ext "jellyfin-icon-c.png" (jellyfin <> "/web/#/details?id=" <> j) "Jellyfin"
              _ -> [])

    -- tracks, credits and review from /api/albumtq, once they are there and belong to this album
    tj = case state.albumT of
      Just t | t.tjId == (if verIsFile then state.version else a.albumID) -> Just t
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
            , case tracks >>= _.atReviewUrl of
                -- a Wikipedia lead (CC BY-SA): say whom it is about, and link the article
                Just url ->
                  HH.p [ cls "av-src" ]
                    [ HH.text (maybe "From " (\n -> "About " <> n <> ", from ") (tracks >>= _.atReviewAbout))
                    , HH.a [ HP.href url, HP.target "_blank", HP.rel "noopener" ] [ HH.text "Wikipedia" ]
                    , HH.text " (CC BY-SA)"
                    ]
                Nothing ->
                  HH.p [ cls "av-src" ] [ HH.text (fromMaybe "Review" (tracks >>= _.atReviewSource) <> ", via Tidal") ]
            ]
        ]
      _ -> []

    -- the track list, with a line per disc and per work; ↻ fetches it again
    tracksView =
      [ HH.div [ cls "av-tracks" ] $
          [ HH.div [ cls "av-sect-head" ] $
              [ HH.h2_ [ HH.text "Tracks" ] ]
              <> (case tracks of
                    Just t -> [ HH.span_ [ HH.text ("from " <> t.atSource <> maybe "" (" · " <> _) t.atQuality <> " · " <> S.take 10 t.atFetched) ] ]
                    Nothing -> [])
              -- whose tracks: the version chosen in Versions
              <> [ HH.button [ cls "av-tag", HE.onClick \_ -> RefreshTracks, HP.disabled state.tracksLoading
                             , HP.title "Fetch tracks, credits and review again" ] [ HH.text "↻" ] ]
          ] <> case tracks of
                 Just t -> trackRows t.atTracks
                 Nothing | state.tracksLoading -> [ HH.p [ cls "av-note" ] [ HH.text "Loading tracks…" ] ]
                         | otherwise -> [ HH.p [ cls "av-note" ] [ HH.text (maybe "No tracks." (\t -> "No tracks: " <> t.tjError) tj) ] ]
      ]
    isFile = S.take 1 a.albumID == "J"
    isTidalAlbum = S.take 1 a.albumID == "T"
    trackRows :: Array Track -> Array (H.ComponentHTML Action () m)
    trackRows ts =
      let discs = nub (map _.tkDisc ts)
          multi = length discs > 1
          -- the position column is as wide as the longest position ("CD-12", "DVD-3"), so
          -- that none wraps and the titles stay in line
          posLen = fromMaybe 2 (maximum (map (S.length <<< _.tkPos) ts))
          rowStyle = "grid-template-columns: max(34px, " <> show posLen <> "ch) minmax(0, 1fr) auto"
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
            <> [ HH.div [ cls (if t.tkWork == Nothing then "av-trk" else "av-trk av-mov"), HP.style rowStyle ]
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

    -- what the references say: one fold per reference, credits open (an opera's cast), the
    -- tracklist only when asked for
    refData = case state.refsX of
      Just (Tuple k rs) | k == a.albumID -> rs
      _ -> []
    refDataView =
      if null refData then [] else
        [ HH.div [ cls "av-refs" ] $
            [ HH.h3_ [ HH.text "From Discogs "
                     , HH.button [ cls "av-tag", HE.onClick \_ -> RefreshRefs, HP.title "Fetch from Discogs again" ] [ HH.text "↻" ] ] ]
            <> map refOne refData
        ]
    refOne rf = case rf.rfData of
      Nothing -> HH.p [ cls "av-note" ] [ HH.a [ HP.href rf.rfUrl, HP.target "_blank" ] [ HH.text rf.rfRef ], HH.text (": " <> rf.rfError) ]
      Just d ->
        let isMaster = S.take 7 d.drRef == "master/"
            yr = maybe "" show d.drYear
            line = S.joinWith " · " (filter (_ /= "")
                     [ S.trim (d.drLabel <> " " <> d.drCatno), d.drFormat, d.drCountry
                     , (if isMaster then maybe "" (\m -> "first " <> show m) d.drMasterYear else "")
                     , (if isMaster && d.drMasterYear == d.drYear then "" else yr) ])
        in
        HH.div [ cls "av-ref" ] $
          [ HH.p_ [ HH.a [ HP.href d.drUrl, HP.target "_blank", HP.title "Open on discogs.com" ]
                        [ HH.text ((if isMaster then "Master · " else "Release · ") <> d.drArtist <> " — " <> d.drTitle) ] ]
          , HH.p [ cls "av-note" ] [ HH.text (line <> (if isMaster then " (main release " <> show d.drRelease <> ")" else "")) ]
          ]
          <> (if null d.drCredits then [] else
                [ HH.details [ cls "av-credits", HP.attr (HH.AttrName "open") "" ]
                    [ HH.summary_ [ HH.text ("Credits (" <> show (length d.drCredits) <> ")") ]
                    , HH.div [ cls "av-facts" ] (concat (map (\c -> row c.crRole [ HH.text (S.joinWith ", " c.crNames) ])
                                                            (filter (not <<< isOther) d.drCredits <> filter isOther d.drCredits)))
                    ] ])
          <> (if null d.drTracks then [] else
                [ HH.details [ cls "av-credits" ]
                    [ HH.summary_ [ HH.text ("Tracklist (" <> show (length d.drTracks) <> ")") ]
                    , HH.div [ cls "av-tracks" ] (trackRows d.drTracks)
                    ] ])

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
          | f == "Jellyfin" = 'f'
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
    Just jid -> if a.albumFormat == "Jellyfin" || state.menu.params.mjellyfin == "" then HH.div_ [] else -- don't show if just Jellyfin
      HH.div [ HP.class_ $ HH.ClassName "cover-obackground4"]
      [ HH.a
        [ HP.href (state.menu.params.mjellyfin <> "/web/#/details?id=" <> jid)]
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
