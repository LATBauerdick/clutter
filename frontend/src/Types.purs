module Types  ( Album
              , ArtistRef
              , AlbumFacts
              , AlbumRef
              , Related
              , AlbumXJ
              , RefJ
              , DiscogsRefJ
              , FileV
              , CandV
              , Curated
              , ReviewRow
              , ReviewJ
              , Track
              , Credit
              , AlbumTracks
              , AlbumTJ
              , AlbumJ
              , AlbumsJ
              , ListenedJ
              , SortOrder (..)
              , State
              , MenuState
              , MenuParams
              , AlbumList (..)
              , ParamsJ
              , Action (..)
              , View (..)
              , pLocList
              , pageSize
              ) where

import Prelude (class Eq, class Show, (<<<), ($))
import Data.Newtype
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple)
import Data.Set (Set)
import Data.DateTime (DateTime)
import Web.Event.Event (Event)
-- import Web.UIEvent.MouseEvent (MouseEvent)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.Utils ( words )
import Data.Array ( head )

data SortOrder = Asc | Desc
derive instance eqSortOrder :: Eq SortOrder
derive instance genericSortOrder :: Generic SortOrder _
instance showSortOrder :: Show SortOrder where
  show = genericShow

-- a long list draws this many cards at first, and this many more each step
pageSize :: Int
pageSize = 240

pLocList :: AlbumList -> Boolean -- lists with location info
pLocList (AlbumList xx) = maybe false xxx xx where
 xxx :: String -> Boolean
 xxx n = case head <<< words $ n of
  Just "Basement" -> true
  Just "Box" -> true
  Just "Cube" -> true
  Just "Shelf" -> true
  Just "Incoming" -> true
  Just "Lost&Found" -> true
  _ -> false

type Album =      { albumID :: String
                  , albumAMusic :: Maybe String
                  , albumTidal :: Maybe String
                  , albumJellyfin :: Maybe String
                  , albumAdded :: Maybe String
                  , albumArtist :: String
                  , albumTitle :: String
                  , albumCover :: String
                  , albumURL :: String
                  , albumFormat :: String
                  , albumLocation :: Maybe String
                  , albumLocIdx :: Maybe Int
                  , albumTags :: Array String
                  , albumFolder :: Int
                  , albumPlays :: Int
                  , albumListenedDates :: Array String
                  , albumRating :: Int
                  , albumReleased :: String
                  , albumFavoriteUntil :: Maybe String -- removed from the Tidal collection: when
                  -- Wikipedia/AllMusic links from wikidata.json (backend Wikidata.hs); Maybe, so
                  -- an older backend without them still decodes
                  , albumWiki :: Maybe String
                  , albumAllMusic :: Maybe String
                  , albumArtists :: Maybe (Array ArtistRef)
                  , albumShelf :: Maybe (Tuple String Int)
                  , albumPlayedAt :: Maybe String -- set in the Listened views: the time of this play
                  , albumVersions :: Maybe (Array String) -- set from the list's lversions: the album's versions ("Vinyl", "Tidal", "Files · imspl"); none or [] for an album of one
                  }
type ArtistRef =  { arName :: String -- without the Discogs "(n)" suffix
                  , arKey :: String
                  , arWiki :: Maybe String
                  , arAllMusic :: Maybe String
                  }
-- /api/albumxq: the album page's details (backend Details.hs, Related.hs)
type AlbumFacts = { afLength :: Maybe Int -- seconds
                  , afTracks :: Maybe Int
                  , afQuality :: Maybe String
                  , afLabel :: Maybe String
                  , afReleased :: Maybe String -- the full date
                  , afKind :: Maybe String -- "EP", "Single"
                  , afExplicit :: Boolean
                  , afStyles :: Array String
                  , afGone :: Maybe String -- since when the album is known to be off Tidal
                  }
type AlbumRef =   { rId :: String
                  , rTitle :: String
                  , rArtist :: String
                  , rCover :: String
                  , rYear :: String
                  , rFormat :: String
                  , rNote :: String
                  }
type Related =    { relVersions :: Array AlbumRef
                  , relByArtist :: Array AlbumRef
                  , relSimilar :: Array AlbumRef
                  , relSimilarName :: String
                  }
type AlbumXJ =    { xid :: String
                  , xfacts :: AlbumFacts
                  , xrelated :: Related
                  , xloc :: Maybe (Tuple String Int) -- location list and position in it
                  , xstreams :: Maybe (Array String) -- Tidal / Apple albums linked to this release in clutter's log (T…, A…)
                  , xcandidates :: Maybe (Array CandV) -- for a file album: albums it may be a version of
                  , xfiles :: Maybe (Array FileV) -- file albums on the server that are versions of this album
                  , xnode :: Maybe Boolean -- the album's files are in a share the Node has indexed (▶ can play them)
                  , xof :: Maybe CandV -- a version that is not its album's lead: the album's row
                  , xnotsame :: Maybe (Array CandV) -- suggestions marked "not the same album" (×)
                  , xversions :: Maybe (Array CandV) -- every other version of the album (a stream clutter does not hold: key only, cvTitle "")
                  , xlead :: Maybe String -- the album's lead version (its row)
                  , xleadSet :: Maybe Boolean -- a lead line chose it, not the rule
                  , xmembers :: Maybe (Array String) -- the versions the page may take off or let lead; none for an album of one version
                  , xrefs :: Maybe (Array String) -- Discogs entries that describe the album, not owned: "master/…", "release/…"
                  , xquality :: Maybe String -- a file album: its files' quality, once its tracks are in the cache ("FLAC 24/96")
                  , xname :: Maybe String -- the album's name: its C id, or the key itself for an album of one version
                  }
-- /api/albumrefq: one Discogs reference with what Discogs says (Nothing: rfError says why)
type RefJ =       { rfRef :: String
                  , rfUrl :: String
                  , rfData :: Maybe DiscogsRefJ
                  , rfError :: String
                  }
type DiscogsRefJ = { drRef :: String
                   , drUrl :: String
                   , drRelease :: Int -- the release the data is from (a master: its main release)
                   , drTitle :: String
                   , drArtist :: String
                   , drYear :: Maybe Int
                   , drMasterYear :: Maybe Int
                   , drLabel :: String
                   , drCatno :: String
                   , drCountry :: String
                   , drFormat :: String
                   , drTracks :: Array Track
                   , drCredits :: Array Credit
                   , drCover :: Maybe String -- the release's main image (an album with no cover of its own shows it)
                   }
-- an album a file album may be a version of
type CandV =      { cvKey :: String
                  , cvTitle :: String
                  , cvArtist :: String
                  , cvFormat :: String
                  , cvReleased :: String
                  , cvHas :: Maybe String -- the release's present album of that service, "" if none
                  }
-- /api/curated: a curated tag ("my.piano"), its name ("Piano"), and how many album rows have it
type Curated =    { ctTag :: String
                  , ctName :: String
                  , ctCount :: Int
                  }
-- /api/reviewq: a file album the inventory could not link for sure, with the albums it may
-- be a version of (the inventory's choice first)
type ReviewRow =  { rrKey :: String
                  , rrTitle :: String
                  , rrArtist :: String
                  , rrYear :: String
                  , rrMins :: String
                  , rrStorage :: String
                  , rrFolder :: String -- its folder on the server, the last part
                  , rrUrl :: String -- the album in Jellyfin
                  , rrWhy :: String -- why it was not linked for sure
                  , rrCands :: Array CandV
                  }
type ReviewJ =    { rvRows :: Array ReviewRow
                  , rvDone :: Int -- already decided
                  , rvError :: String
                  }
-- a file album (Jellyfin) as a version of another album
type FileV =      { fvKey :: String
                  , fvTitle :: String
                  , fvArtist :: String
                  , fvStorage :: String
                  , fvUrl :: String
                  , fvLogged :: Boolean -- linked in clutter's log (× takes it off); false: named in the Discogs Location field
                  , fvOwner :: Maybe String -- another release of this album holds the link (same Discogs master)
                  , fvOwnerFormat :: Maybe String
                  , fvQuality :: Maybe String -- its files' quality, once its tracks are in the cache
                  , fvNode :: Maybe Boolean -- the player has its folder (▶ can play it); an older backend does not say
                  }
-- /api/albumtq: tracks, credits and review, fetched on demand and cached per album (backend Tracks.hs)
type Track =      { tkDisc :: Int
                  , tkPos :: String
                  , tkWork :: Maybe String -- the work this track is a movement of
                  , tkTitle :: String
                  , tkSecs :: Maybe Int
                  , tkArtists :: Array String
                  }
type Credit =     { crRole :: String
                  , crNames :: Array String
                  }
type AlbumTracks = { atSource :: String -- tidal | discogs
                   , atFetched :: String
                   , atTracks :: Array Track
                   , atCredits :: Array Credit
                   , atReview :: Maybe String
                   , atReviewSource :: Maybe String -- "TiVo" (via Tidal) or "Wikipedia"
                   , atReviewUrl :: Maybe String -- the Wikipedia article
                   , atReviewAbout :: Maybe String -- the artist the text is about; Nothing: the album
                   , atQuality :: Maybe String -- the files' format, "FLAC 24/96"; from Jellyfin only
                   }
type AlbumTJ =    { tjId :: String
                  , tjTracks :: Maybe AlbumTracks
                  , tjError :: String
                  }
type AlbumJ =     { aid :: String
                  , album :: Album
                  }

type AlbumsJ =    { listName :: String
                  , lalbums :: Array (Tuple Album (Maybe (Tuple String Int)))
                  , lversions :: Maybe (Array (Array String)) -- row by row (an older backend: none)
                  }

-- /api/listenedq: AlbumsJ, one row per play, plus the time of each play
type ListenedJ =  { listName :: String
                  , lalbums :: Array (Tuple Album (Maybe (Tuple String Int)))
                  , lplayed :: Array String
                  , lversions :: Maybe (Array (Array String))
                  }

newtype AlbumList = AlbumList (Maybe  String)
derive instance newtypeAlbumList :: Newtype AlbumList _

type State =      { apiUrl :: String
                  -- one view per browser history entry of this page, and which of them is shown;
                  -- the entry at pos is filled in when that view is left (Back, Forward, ‹ ›)
                  -- the frame: which click-open menu is open (focus | sort | more), the sidebar as a
                  -- drawer on a narrow screen, the search in the list on show, the filter of the tag picker
                  , menuOpen :: Maybe String
                  , confirmDrop :: Boolean -- "Remove from clutter" was clicked: ask once more
                  , notice :: Maybe String -- the answer to the last update, shown in the top bar
                  , sideOpen :: Boolean
                  , listenedOpen :: Boolean -- the sidebar's Listened fold, while a Listened list is on show
                  , search :: String
                  , tagFilter :: String
                  , streamRef :: String -- the album page's "link a stream" box: T<id>, A<id> or a share link
                  , refsX :: Maybe (Tuple String (Array RefJ)) -- album id, what its Discogs references say (/api/albumrefq)
                  , findQ :: String -- the album page's "Same album as…" search box
                  , found :: Array CandV -- what that search found (/api/albumfind)
                  , curated :: Array Curated -- the curated tags clutter offers (/api/curated): the sidebar's views, the album page's picker
                  , views :: Array View
                  , pos :: Int
                  , album :: Maybe Album
                  , albumX :: Maybe AlbumXJ -- details of that album, loaded after it
                  , albumT :: Maybe AlbumTJ -- its tracks, credits and review, loaded last (may take a moment)
                  , tracksLoading :: Boolean
                  , version :: String -- the album page's version (Versions block): "" the album itself, "tidal" its Tidal album (a Discogs release), "J…" one of its file albums; the facts, tracks and ▶ follow it; back to "" with each album
                  , verX :: Maybe AlbumXJ -- a file-album version's details (/api/albumxq/J…)
                  , verA :: Maybe Album -- a file-album version itself (/api/albumq/J…)
                  , aboutLong :: Boolean -- the review is longer than the lines shown, so "more" is offered
                  , aboutOpen :: Boolean -- the review is shown in full; folded again with each album
                  , listName :: AlbumList
                  , albumList :: Array Album
                  , review :: Maybe ReviewJ -- the review of the unsure matches is on show
                  , playFiles :: Boolean -- an album with a Tidal version and files: ▶ plays the files (the album page's chooser)
                  , selecting :: Boolean -- a list in select mode: a click on a cover selects it (bulk Keep)
                  , selected :: Set String -- the album ids selected; emptied whenever the list is loaded
                  , shownMax :: Int -- a long list draws this many cards; more as the page is scrolled down
                  , loading :: Boolean
                  , albumID :: String
                  , now :: DateTime
                  , result :: Maybe String
                  , nodeMsg :: Maybe String -- answer to the last ▶ Node
                  , nodeName :: String -- the Bluesound player's name, "Node" until known
                  , menu :: MenuState
                  , sortKept :: Maybe (Tuple String SortOrder) -- the other lists' sort, while a Listened list (which opens in play order) is on show
                  }
type MenuState =  { params :: MenuParams
                  , ln :: String
                  , ffs :: Array (Tuple String Boolean)
                  , sortName :: String
                  , sso :: SortOrder
                  }
type MenuParams = { muhq :: String
                  , msorts :: Array String
                  , msts :: Array String
                  , mlistNames :: Array String
                  , mlocNames :: Array String
                  , mjellyfin :: String -- Jellyfin as the browser reaches it; "" if there is none
                  }
type ParamsJ =    { timeStamp :: String
                  , params :: MenuParams
                  }

-- a view to go back to: an album, or a list with its focus and sort, or the review
data View = VAlbum String
          | VList AlbumList String (Array (Tuple String Boolean)) String SortOrder
          | VReview

data Action = ToggleSortOrder
            | SetSortOrder SortOrder
            | SetSort String
            | ToggleFocus String
            | SetFocus (Array (Tuple String Boolean))
            | SetAlbumID String
            | MakeRequest Event
            | ShowList AlbumList
            | ShowListSort AlbumList String SortOrder
            | ShowAlbum String
            | AlbumPlayed String
            | NodePlay String String -- Tidal album id, mode (replace|now|next|last): play it on the Bluesound Node
            | NodePlayFiles String String -- album id, mode: play its files on the Node from the player's own library
            | SetPlayFiles Boolean -- the album page's chooser: ▶ plays the files (true) or the Tidal version
            | ShowFocus String -- a tag: show all albums that carry it
            | NodeCtl String -- toggle | skip | stop
            | LoadNodeName -- on start: ask the backend for the player's name
            | UpdateDiscogs
            | SetStreamRef String
            | LinkStream String -- release id: link the stream named in the box
            | AddRef String String -- album id, a Discogs release or master (address or release/…): it describes the album
            | DropRef String String -- album id, reference ("release/…"): take it off
            | RefreshRefs -- fetch what the album's references say again
            | LinkStreamTo String String -- release id, stream key (T…, A…): from the stream album's page
            | UnlinkStream String String -- release id, stream key (T…, A…)
            | LinkFile String String -- album id (D…, T…), file album id (J…): the files are a version of that album
            | UnlinkFile String String -- album id, file album id (J…): these files are not this album
            | KeepAlbum String String -- album id; yes | no | unset: the keep mark (docs/jellyfin-plan.md)
            | ToggleSelect -- a list: select mode on or off
            | SelectAlbum String -- in select mode: this album in or out of the selection
            | SelectShown -- everything the list shows (what the search box lets through)
            | ShowMore -- a long list: draw the next cards (scrolled near the end, or the button)
            | SelectNone
            | KeepSelected String -- yes | no | unset: the keep mark for the selection
            | ShowReview -- the review of the inventory's unsure matches
            | ReviewLink String String -- album id, file album id (J…): link them, the row goes
            | ReviewSkip String -- file album id (J…): none of the albums offered; the row goes
            | AskDrop Boolean -- "Remove from clutter": ask (true), or never mind (false)
            | DropAlbum String -- remove this stream album from clutter, then show the Tidal list
            | UpdateServerFull -- the same, asking Jellyfin for every album: finds what left or moved; minutes
            | UpdateServer -- take in what Jellyfin has now (new and gone file albums), then show the server list
            | UpdateTidal -- take in new favourites and the Save for later list, then show the Tidal list
            | UpdateAlbum String -- the album page: its versions asked for again at Jellyfin, Tidal, Discogs (seconds)
            | ToggleAbout -- the review: all of it, or its first lines
            | NotSame String String Boolean -- album, suggestion: not the same album (true), or bring it back (false)
            | JoinAlbum String String -- album, other album: one album now (album layer); then its row is shown
            | SplitVersion String String -- album, version: take that version off, an album of its own again
            | LeadVersion String String -- album, version: that version leads the album; "": back to the rule
            | TagAlbum String String Boolean -- album, curated tag: put it on (true) or take it off (false)
            | RateAlbum String Int -- album, stars 1–5; 0 takes the rating off
            | ShowCurated String -- the view of a curated tag: All, focused on that tag
            | LoadCurated -- fetch the curated tags and their counts again
            | SetFindQ String
            | FindAlbums String -- album: search any album for "Same album as…"
            | ShowVersion String -- the album page shows this version: "" the album, "tidal", "J…" (see version)
            | RefreshTracks -- fetch the tracks, credits and review of the album again
            | TogglePop String -- open or close a menu: focus | sort | more
            | ClosePops -- a click beside the menus: close them and the drawer
            | ListenedClick String -- the sidebar's "Listened": show that list (this year's) and open the fold; on a Listened list: open or close it
            | ToggleSide -- the sidebar as a drawer (narrow screens)
            | SetSearch String
            | SetTagFilter String
            | Back -- the ‹ button: the same as the browser's Back
            | Forward -- the › button: the same as the browser's Forward
            | GoTo Int -- the browser moved to this history entry: show its view
