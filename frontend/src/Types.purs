module Types  ( Album
              , AlbumJ
              , SortOrder (..)
              , MenuState
              ) where

import Data.Maybe (Maybe)

data SortOrder = Asc | Desc
-- derive instance enumSortOrder :: Enum SortOrder

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

type MenuState =  { uhq :: String
                  , ln :: String
                  , ffs :: Array String
                  , sorts :: Array String
                  , sortName :: String
                  , sso :: SortOrder
                  , sts :: Array String
                  , listNames :: Array String
                  , locNames :: Array String
                  }