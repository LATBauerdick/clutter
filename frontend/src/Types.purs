module Types  ( Album
              , AlbumJ
              , AlbumsJ
              , SortOrder (..)
              , State
              , MenuState
              , MenuParams
              , AlbumList (..)
              , ParamsJ
              , Action (..)
              ) where

import Prelude (class Eq, class Show)
import Data.Newtype
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.DateTime (DateTime)
import Web.Event.Event (Event)
-- import Web.UIEvent.MouseEvent (MouseEvent)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data SortOrder = Asc | Desc
derive instance eqSortOrder :: Eq SortOrder
derive instance genericSortOrder :: Generic SortOrder _
instance showSortOrder :: Show SortOrder where
  show = genericShow

type Album =      { albumID :: Int
                  , albumAMusic :: Maybe String
                  , albumTidal :: Maybe String
                  , albumAdded :: Maybe String
                  , albumArtist :: String
                  , albumTitle :: String
                  , albumCover :: String
                  , albumURL :: String
                  , albumFormat :: String
                  , albumLocation :: Maybe String
                  , albumTags :: Array String
                  , albumFolder :: Int
                  , albumPlays :: Int
                  , albumRating :: Int
                  , albumReleased :: String
                  , albumShelf :: Maybe (Tuple String Int)
                  }
type AlbumJ =     { aid :: Int
                  , album :: Album
                  }

type AlbumsJ =    { listName :: String
                  , lalbums :: Array (Tuple Album (Maybe (Tuple String Int)))
                  }

newtype AlbumList = AlbumList (Maybe  String)
derive instance newtypeAlbumList :: Newtype AlbumList _

type State =      { apiUrl :: String
                  , album :: Maybe Album
                  , listName :: AlbumList
                  , albumList :: Array Album
                  , loading :: Boolean
                  , albumID :: String
                  , now :: DateTime
                  , result :: Maybe String
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
