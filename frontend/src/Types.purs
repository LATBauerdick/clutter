module Types  ( Album
              , ArtistRef
              , AlbumFacts
              , AlbumRef
              , Related
              , AlbumXJ
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
              ) where

import Prelude (class Eq, class Show, (<<<), ($))
import Data.Newtype
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple)
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
                  }

-- /api/listenedq: AlbumsJ, one row per play, plus the time of each play
type ListenedJ =  { listName :: String
                  , lalbums :: Array (Tuple Album (Maybe (Tuple String Int)))
                  , lplayed :: Array String
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
                  , views :: Array View
                  , pos :: Int
                  , album :: Maybe Album
                  , albumX :: Maybe AlbumXJ -- details of that album, loaded after it
                  , albumT :: Maybe AlbumTJ -- its tracks, credits and review, loaded last (may take a moment)
                  , tracksLoading :: Boolean
                  , tracksTidal :: Boolean -- a Discogs release: show its Tidal version's tracks; off again with each album
                  , aboutLong :: Boolean -- the review is longer than the lines shown, so "more" is offered
                  , aboutOpen :: Boolean -- the review is shown in full; folded again with each album
                  , listName :: AlbumList
                  , albumList :: Array Album
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
                  }
type ParamsJ =    { timeStamp :: String
                  , params :: MenuParams
                  }

-- a view to go back to: an album, or a list with its focus and sort
data View = VAlbum String
          | VList AlbumList String (Array (Tuple String Boolean)) String SortOrder

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
            | ShowFocus String -- a tag: show all albums that carry it
            | NodeCtl String -- toggle | stop
            | LoadNodeName -- on start: ask the backend for the player's name
            | UpdateDiscogs
            | AskDrop Boolean -- "Remove from clutter": ask (true), or never mind (false)
            | DropAlbum String -- remove this stream album from clutter, then show the Tidal list
            | UpdateTidal -- take in new favourites and the Save for later list, then show the Tidal list
            | ToggleAbout -- the review: all of it, or its first lines
            | TracksSource Boolean -- a Discogs release: its own tracks (false) or the Tidal version's (true)
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
