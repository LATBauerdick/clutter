{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Types ( Tidal (..)
             , Discogs (..)
             , DiscogsInfo (..)
             , TagFolder (..)
             , pLocList
             , TidalInfo (..)
             , Release (..)
             , Album (..)
             , SortOrder (..)
             , Env (..)
             , EnvR (..)
             ) where

import Relude
-- import qualified Text.Show
import Data.Vector ( Vector )

data TidalInfo = TidalFile FilePath | TidalSession Int Text Text Text
data DiscogsInfo = DiscogsFile FilePath | DiscogsSession Text Text
  deriving Show

class ATags f where toInt :: f -> Int
data TagFolder = TDiscogs | TNotUsed | TTidal | TAll
    deriving (Enum, Read, Show, Eq, Ord)
instance ATags TagFolder where
    toInt = fromEnum

data SortOrder = Asc | Desc
  deriving (Enum, Read, Show, Eq, Ord)

pLocList :: Text -> Bool  -- lists with location info
pLocList n = case viaNonEmpty head . words $ n of
                    Just "Cube"   -> True
                    Just "Shelf"  -> True
                    Just "Incoming"  -> True
                    _             -> False

newtype Tidal = Tidal { getTidal :: TidalInfo }

newtype Discogs = Discogs { getDiscogs :: DiscogsInfo } deriving Show

data Env
  = Env
  { albumsR     :: IORef ( Map Int Album )
  , listNamesR  :: IORef ( Vector Text )
  , listsR      :: IORef ( Map Text (Int, Vector Int) )
  , locsR       :: IORef ( Map Int (Text, Int) )  -- lookup (location, pos) by from albumID
  , sortNameR   :: IORef Text
  , sortOrderR  :: IORef SortOrder
  , discogsR    :: IORef Discogs
  , sorts       :: Vector Text
  , url         :: Text
  , getList     :: Env -> Text -> IO ( Vector Int )
  , getSort     :: Map Int Album -> Text -> (SortOrder -> Vector Int -> Vector Int )
  }

data EnvR
  = EnvR
  { albums     :: Map Int Album
  , lists      :: Map Text (Int, Vector Int)
  , locs       :: Map Int (Text, Int) -- lookup (location, pos) by from albumID
  , listNames  :: Vector Text
  , sortName   :: Text
  , sortOrder  :: SortOrder
  , discogs    :: Discogs
  }

data Release
  = Release
  { daid      :: Int
  , dtitle    :: !Text
  , dartists  :: [Text]
  , dreleased :: !Text
  , dadded    :: !Text
  , dcover    :: !Text
  , dfolder   :: Int
  , dformat   :: [Text]
  , dtidalid  :: Maybe Text
  , damid     :: Maybe Text
  , dlocation :: Maybe Text
  , dtags     :: [Text]
  , drating   :: Int
  , dplays    :: Int
  } deriving (Show)

data Album
  = Album
  { albumID       :: Int
  , albumTitle    :: Text
  , albumArtist   :: Text
  , albumReleased :: Text
  , albumCover    :: Text
  , albumAdded    :: Text
  , albumFolder   :: Int
  , albumURL      :: Text
  , albumFormat   :: Text
  , albumTidal    :: Maybe Text
  , albumAM       :: Maybe Text
  , albumLocation :: Maybe Text
  , albumTags     :: [Text]
  , albumRating   :: Int
  , albumPlays    :: Int
  } deriving (Show)
instance Eq Album where
  (==) a b = albumID a == albumID b
