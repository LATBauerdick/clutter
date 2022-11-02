module Types  ( Album
              , AlbumJ
              , SortOrder (..)
              , State
              , MenuState
              , MenuParams
              , AlbumList (..)
              , ParamsJ
              , Action (..)
              ) where

import Prelude
import Data.Maybe (Maybe)
import Data.DateTime (DateTime)
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent (MouseEvent)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data SortOrder = Asc | Desc

derive instance genericSortOrder :: Generic SortOrder _
instance showSortOrder :: Show SortOrder where
  show = genericShow
  -- show Asc = "Asc"
  -- show Desc = "Desc"

type Album =  { albumID :: Int
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
              }
type AlbumJ = { aid :: Int
              , album :: Album
              }

newtype AlbumList = AlbumList (Maybe  String)

type State =  { album :: Maybe Album
              , list :: AlbumList
              , loading :: Boolean
              , albumID :: String
              , now :: DateTime
              , result :: Maybe String
              , menu :: MenuState
              }
type MenuState = { params :: MenuParams
                 , ln :: String
                 , ffs :: Array String
                 , sortName :: String
                 , sso :: SortOrder
                 }
type MenuParams = { muhq :: String
                  , msorts :: Array String
                  , msts :: Array String
                  , mlistNames :: Array String
                  , mlocNames :: Array String
                  }
type ParamsJ = { timeStamp :: String
               , params :: MenuParams
               }

data Action = Increment | Decrement | SetAlbumID String | MakeRequest Event | ShowList MouseEvent AlbumList
