module Types  ( Album
              , ArtistRef
              , AlbumFacts
              , AlbumRef
              , Related
              , AlbumXJ
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
                  , views :: Array View
                  , pos :: Int
                  , album :: Maybe Album
                  , albumX :: Maybe AlbumXJ -- details of that album, loaded after it
                  , listName :: AlbumList
                  , albumList :: Array Album
                  , loading :: Boolean
                  , albumID :: String
                  , now :: DateTime
                  , result :: Maybe String
                  , nodeMsg :: Maybe String -- answer to the last ▶ Node
                  , nodeName :: String -- the Bluesound player's name, "Node" until known
                  , menu :: MenuState
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
            | Back -- the ‹ button: the same as the browser's Back
            | Forward -- the › button: the same as the browser's Forward
            | GoTo Int -- the browser moved to this history entry: show its view
