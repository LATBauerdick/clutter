{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, InstanceSigs #-}

module Types ( Tidal (..)
             , Discogs (..)
             , DiscogsInfo (..)
             , TagFolder (..)
             , pLocList
             , TidalInfo (..)
             , Release (..)
             , Album (..)
             , SortOrder (..)
             , AppM
             , Env (..)
             , EnvR (..)
             , envGetEnvr
             , envGetDiscogs
             , envGetListName
             ) where

import Relude
import qualified Text.Show
import Data.Vector ( Vector )
import qualified Data.Map.Strict as M
import Servant (Handler)

data TidalInfo = TidalFile FilePath | TidalSession Int Text Text Text
  deriving Show
data DiscogsInfo = DiscogsEnv Env | DiscogsFile FilePath | DiscogsSession Text Text
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

type AppM = ReaderT Env Handler
data Env
  = Env
  { albumsR     :: IORef ( Map Int Album )
  , listNamesR  :: IORef ( Map Text Int )
  , listsR      :: IORef ( Map Text (Int, Vector Int) )
  , locsR       :: IORef ( Map Int (Text, Int) )  -- lookup (location, pos) by from albumID
  , sortNameR   :: IORef Text
  , sortOrderR  :: IORef SortOrder
  , discogsR    :: IORef Discogs
  , tagsR       :: IORef ( Map Text [Int] )
  , focusR      :: IORef [Text]
  , sorts       :: Vector Text
  , url         :: Text
  , getList     :: Text -> AppM ( Vector Int )
  , getSort     :: Map Int Album -> Text -> (SortOrder -> Vector Int -> Vector Int )
  }
instance Show Env where
  show :: Env -> String
  show _ = "This is an Env"

envGetDiscogs :: AppM Discogs
envGetDiscogs = asks discogsR >>= readIORef

envGetListName :: Int -> AppM (Maybe Text)
envGetListName i = do
  lns <- asks listNamesR >>= readIORef
  let ln :: Maybe Text; ln = fmap fst . find (\(_, li) -> li == i) $ M.toList lns
  pure ln

envGetEnvr :: AppM EnvR
envGetEnvr = do
  env <- ask
  am  <- readIORef (albumsR env)
  lm  <- readIORef (listsR env)
  lcs <- readIORef (locsR env)
  lns <- readIORef (listNamesR env)
  sn  <- readIORef (sortNameR env)
  so  <- readIORef (sortOrderR env)
  di  <- readIORef (discogsR env)
  tm  <- readIORef (tagsR env)
  fs  <- readIORef (focusR env)
  pure $ EnvR am lm lcs lns sn so di tm fs

data EnvR
  = EnvR
  { albums     :: Map Int Album
  , lists      :: Map Text (Int, Vector Int)
  , locs       :: Map Int (Text, Int) -- lookup (location, pos) by from albumID
  , listNames  :: Map Text Int
  , sortName   :: Text
  , sortOrder  :: SortOrder
  , discogs    :: Discogs
  , tags       :: Map Text [Int]
  , focus      :: [Text]
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
  , dformat   :: Text
  , dtidalid  :: Maybe Text
  , damid     :: Maybe Text
  , dlocation :: Maybe Text
  , dtags     :: Vector Text
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
  , albumTags     :: Vector Text
  , albumRating   :: Int
  , albumPlays    :: Int
  } deriving (Show)
instance Eq Album where
  (==) a b = albumID a == albumID b
