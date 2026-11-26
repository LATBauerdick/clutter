{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Types (
  Tidal (..),
  AMusic (..),
  Discogs (..),
  TagFolder (..),
  pLocList,
  TidalInfo (..),
  AMusicInfo (..),
  Release (..),
  Album (..),
  ArtistRef (..),
  AlbumFacts (..),
  AlbumKey (..),
  keyText,
  parseKey,
  PlayEvent (..),
  SortOrder (..),
  MenuParams (..),
  AppM,
  Env (..),
  EnvR (..),
  Layer (..),
  rowOf,
  rowOfName,
  nameOf,
  versionsOfKey,
  envGetEnvr,
  envGetDiscogs,
  envGetListName,
) where

import Data.Char (isAlphaNum, isHexDigit, toLower)
import qualified Data.Map.Strict as M
import Data.Vector (Vector)
import Relude
import Servant (Handler)
import qualified Text.Show

import Data.Aeson (FromJSON (..), Options (..), ToJSON (..), Value (String), defaultOptions, genericParseJSON, genericToEncoding, genericToJSON, withText)
import qualified Data.Text as T
import Servant (FromHttpApiData (..))

-- album identity, typed by catalogue: the same number means different albums in
-- Discogs, Tidal, Apple Music and Jellyfin. Written with a prefix, as in the Discogs
-- Location field ("T12571482 A206349046"): D6715388, T283130328, A1556465220, J…
-- A Jellyfin id is its item id, 32 hex digits (kept in lower case), not a number.
-- Q is a Qobuz album id (letters and digits): a stream a release can have, not an album clutter lists.
-- C is an album's own id once it has two or more versions (docs/album-layer-plan.md ›
-- Ids): 6 characters from 0-9 and a-z without i l o u. It lives in the log and the
-- server only; pages and lists name an album by a version key (AlbumLayer).
data AlbumKey = D Int | T Int | A Int | J Text | Q Text | C Text
  deriving (Eq, Ord, Generic)

keyText :: AlbumKey -> Text
keyText k = case k of
  D i -> "D" <> show i
  T i -> "T" <> show i
  A i -> "A" <> show i
  J i -> "J" <> i
  Q i -> "Q" <> i
  C i -> "C" <> i

-- "D123", "T123", "A123", "J<32 hex digits>", "C<6 id characters>"; a bare number is a
-- Discogs release (old links)
parseKey :: Text -> Maybe AlbumKey
parseKey t = case T.uncons t of
  Just ('C', n) | T.length n == 6 && T.all (`elem` ("0123456789abcdefghjkmnpqrstvwxyz" :: String)) n -> Just (C n)
  Just ('C', _) -> Nothing
  Just ('D', n) -> D <$> num n
  Just ('T', n) -> T <$> num n
  Just ('A', n) -> A <$> num n
  Just ('J', n) | T.length n == 32 && T.all isHexDigit n -> Just (J (T.toLower n))
  Just ('J', _) -> Nothing
  Just ('Q', n) | not (T.null n) && T.all (\c -> isAlphaNum c || c == '-' || c == '_') n -> Just (Q n)
  Just ('Q', _) -> Nothing
  _ -> D <$> num t
 where
  num = readMaybe . toString

instance Show AlbumKey where
  show = toString . keyText
instance ToJSON AlbumKey where
  toJSON = String . keyText
instance FromJSON AlbumKey where
  parseJSON = withText "AlbumKey" $ maybe (fail "not an album key") pure . parseKey
instance FromHttpApiData AlbumKey where
  parseUrlPiece t = maybe (Left ("not an album key: " <> t)) Right (parseKey t)

data TidalInfo = TidalFile FilePath | TidalSession Int Text Text Text
  deriving (Show)
data AMusicInfo = AMusicSession Text Text
  deriving (Show)
data Discogs = DiscogsFile FilePath | DiscogsSession Text Text
  deriving (Show)

data TagFolder = TDiscogs | TNotUsed | TTidal | TAll | TAMusic | TJellyfin
  deriving (Enum, Read, Show, Eq, Ord)

-- class ATags f where toInt :: f -> Int
-- instance ATags TagFolder where
--     toInt = fromEnum

data SortOrder = Asc | Desc
  deriving (Enum, Read, Show, Eq, Ord)

data MenuParams = MenuParams
  { muhq :: Text
  , msorts :: Vector Text
  , msts :: [Text]
  , mlistNames :: [Text]
  , mlocNames :: [Text]
  , mjellyfin :: Text -- Jellyfin as a browser reaches it; "" if there is none
  }
  deriving (Eq, Show, Generic)
instance ToJSON MenuParams

pLocList :: Text -> Bool -- lists with location info
pLocList n = case viaNonEmpty head . words $ n of
  Just "Basement" -> True
  Just "Box" -> True
  Just "Cube" -> True
  Just "Shelf" -> True
  Just "Incoming" -> True
  Just "Lost&Found" -> True
  _ -> False

newtype Tidal = Tidal {getTidal :: TidalInfo}

newtype AMusic = AMusic {getAMusic :: AMusicInfo}

-- newtype Discogs = Discogs { getDiscogs :: DiscogsInfo } deriving Show

type AppM = ReaderT Env Handler
data Env = Env
  { albumsR :: IORef (Map AlbumKey Album)
  , listNamesR :: IORef (Map Text Int)
  , listsR :: IORef (Map Text (Int, Vector AlbumKey))
  , locsR :: IORef (Map AlbumKey (Text, Int)) -- lookup (location, pos) by from albumID
  , playsR :: IORef [PlayEvent] -- the play log, oldest first
  , sortNameR :: IORef Text
  , sortOrderR :: IORef SortOrder
  , discogsR :: IORef Discogs
  , tagsR :: IORef (Map Text [AlbumKey])
  , focusR :: IORef [Text]
  , sorts :: Vector Text
  , url :: Text
  , getList :: Text -> AppM (Vector AlbumKey)
  , getSort :: Map AlbumKey Album -> Text -> (SortOrder -> Vector AlbumKey -> Vector AlbumKey)
  , -- Wikidata.attachWiki and Details.factsFor with their stores as last read: at start, and
    -- again by "Update from Tidal" (Env.envTidalReload), which may have extended them
    wikiLinksR :: IORef (Map AlbumKey Album -> Map AlbumKey Album)
  , albumFactsR :: IORef (Album -> AlbumFacts)
  , jellyfinWeb :: Text -- Jellyfin as a browser reaches it (Jellyfin.jsWeb); "" if there is none
  , -- the albums as the readers give them (Discogs, Tidal, Apple Music, Jellyfin), before
    -- the album layer, the plays and the stores are put on: albumsR is computed from these
    -- (Env.rebuild), so a version that leaves an album leaves nothing behind
    baseR :: IORef (Map AlbumKey Album)
  , layerR :: IORef Layer -- the album layer as the collection log and the albums held leave it
  , shelfTagsR :: IORef (Map Text [AlbumKey]) -- the tags index for a record's row in a shelf list (tagsR: an album's row)
  , standInTags :: Set Text -- folder tags of the Discogs stand-in folders (Collection.standInFolderTags)
  }

-- The album layer as the server uses it (Collection.buildLayer; docs/album-layer-plan.md).
-- An album is named by its C id, or by its one version's key. Its row in every list but a
-- shelf is its lead version's key.
data Layer = Layer
  { lyFormat :: Bool -- the log is in the album format (it has the format line)
  , lyName :: Map AlbumKey AlbumKey -- every key clutter holds -> its album's name
  , lyRow :: Map AlbumKey AlbumKey -- a key clutter holds whose album's row is another key -> that key
  , lyVersions :: Map AlbumKey [AlbumKey] -- an album with a C id -> its versions in the log, held or not; those held first, the lead first
  , lyKeep :: Map AlbumKey Bool -- an album's keep mark, by name
  , lyStandIns :: Map AlbumKey [AlbumKey] -- an album -> the stand-in releases clutter holds that the log reads as one of its versions
  , lyIds :: Set AlbumKey -- every C id the log has used (a new one must be none of them)
  , lyPlay :: PlayEvent -> Maybe AlbumKey -- the album a play counts for, if clutter holds its version
  }

-- the key of a held album's row: its lead version's
rowOf :: Layer -> AlbumKey -> AlbumKey
rowOf ly k = M.findWithDefault k k (lyRow ly)

-- the row of an album by its name (Layer.lyPlay gives names): its lead's key
rowOfName :: Layer -> AlbumKey -> AlbumKey
rowOfName ly n = case M.lookup n (lyVersions ly) of
  Just (l : _) -> rowOf ly l
  _ -> rowOf ly n

-- an album's name (C id or a key) for a key clutter holds
nameOf :: Layer -> AlbumKey -> AlbumKey
nameOf ly k = M.findWithDefault k k (lyName ly)

-- the versions of the album a held key belongs to, as the log has them, lead first
versionsOfKey :: Layer -> AlbumKey -> [AlbumKey]
versionsOfKey ly k = M.findWithDefault [k] (nameOf ly k) (lyVersions ly)
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
  am <- readIORef (albumsR env)
  lm <- readIORef (listsR env)
  lcs <- readIORef (locsR env)
  lds <- readIORef (playsR env)
  lns <- readIORef (listNamesR env)
  sn <- readIORef (sortNameR env)
  so <- readIORef (sortOrderR env)
  di <- readIORef (discogsR env)
  tm <- readIORef (tagsR env)
  fs <- readIORef (focusR env)
  pure $ EnvR am lm lcs lds lns sn so di tm fs

data EnvR = EnvR
  { albums :: Map AlbumKey Album
  , lists :: Map Text (Int, Vector AlbumKey)
  , locs :: Map AlbumKey (Text, Int) -- lookup (location, pos) by from albumID
  , plays :: [PlayEvent] -- the play log, oldest first
  , listNames :: Map Text Int
  , sortName :: Text
  , sortOrder :: SortOrder
  , discogs :: Discogs
  , tags :: Map Text [AlbumKey]
  , focus :: [Text]
  }

data Release = Release
  { daid :: Int
  , dinst :: Int
  , dtitle :: !Text
  , dartists :: [Text]
  , dartistids :: [Text] -- Wikidata store keys, parallel to dartists: "da:<id>", "ta:<id>", "" if none
  , dmaster :: Maybe Int -- Discogs master id
  , dreleased :: !Text
  , dadded :: !Text
  , dcover :: !Text
  , dfolder :: Int
  , dformat :: Text
  , dtidalid :: Maybe Text
  , damid :: Maybe Text
  , dqobuzid :: Maybe Text
  , djellyfinid :: Maybe Text
  , dlocIdx :: Maybe Int
  , dlocation :: Maybe Text
  , dtags :: [Text]
  , drating :: Int
  , dplays :: Int
  , dfavuntil :: Maybe Text -- removed from the Tidal collection: when
  }
  deriving (Show)

data Album = Album
  { albumID :: AlbumKey
  , albumInst :: Int
  , albumTitle :: Text
  , albumArtist :: Text
  , albumReleased :: Text
  , albumCover :: Text
  , albumAdded :: Text
  , albumFolder :: Int
  , albumURL :: Text
  , albumFormat :: Text
  , albumTidal :: Maybe Text
  , albumAMusic :: Maybe Text
  , albumQobuz :: Maybe Text
  , albumJellyfin :: Maybe Text
  , albumLocIdx :: Maybe Int
  , albumLocation :: Maybe Text
  , albumTags :: [Text]
  , albumRating :: Int
  , albumPlays :: Int -- Discogs collection field 7 "Plays"
  , albumListenedDates :: [Text] -- play times from clutter's play log, newest first
  , albumFavoriteUntil :: Maybe Text -- removed from the Tidal collection: when
  , albumWikiKeys :: [Text] -- Wikidata store keys of the album, best first: "dm:<id>", "t:<id>", "a:<id>"
  , albumWiki :: Maybe Text -- Wikipedia page (English, else German), from wikidata.json
  , albumAllMusic :: Maybe Text -- AllMusic page
  , albumArtists :: [ArtistRef] -- one per artist, with its links
  , albumFiles :: [Text] -- Jellyfin ids of the file albums that are versions of this album: named in the
  -- Discogs Location field, or linked in the collection log (Collection.attachLinks)
  }
  deriving (Show, Generic)

-- an album's artist, with links from wikidata.json (Wikidata.attachWiki)
data ArtistRef = ArtistRef
  { arName :: Text -- display name, without the Discogs "(n)" suffix
  , arKey :: Text -- Wikidata store key, "da:<id>" or "ta:<id>", "" if none
  , arWiki :: Maybe Text
  , arAllMusic :: Maybe Text
  }
  deriving (Show, Generic)
instance ToJSON ArtistRef

-- facts about an album that are on disk but not in Album (Details.hs; docs/album-page-plan.md,
-- phase 1). Sent by /api/albumxq, not with every Album in a list.
data AlbumFacts = AlbumFacts
  { afLength :: Maybe Int -- seconds
  , afTracks :: Maybe Int
  , afQuality :: Maybe Text -- "Hi-Res lossless", "Lossless", "Dolby Atmos"
  , afLabel :: Maybe Text -- Discogs label and catalog number, or Tidal's copyright line
  , afReleased :: Maybe Text -- the full date, "2025-09-26"
  , afKind :: Maybe Text -- "EP", "Single"; Nothing for an album
  , afExplicit :: Bool
  , afStyles :: [Text] -- Discogs styles
  , afGone :: Maybe Text -- since when the album is known to be off Tidal
  }
  deriving (Show, Generic)
instance ToJSON AlbumFacts

instance Eq Album where
  (==) a b = albumID a == albumID b
instance ToJSON Album

-- one line of clutter's play log, listened/YYYY.jsonl (see Listened.hs)
data PlayEvent = PlayEvent
  { peTs :: Text -- local time with UTC offset, or a bare date for imported date-only plays
  , peSrc :: Text -- discogs|tidal|apple|qobuz|jellyfin
  , peDiscogs :: Maybe Int
  , peInst :: Maybe Int
  , peTidal :: Maybe Int
  , peApple :: Maybe Int
  , peApplelib :: Maybe Text -- Apple Music library id, for albums without a catalog id
  , peJellyfin :: Maybe Text -- Jellyfin item id; it changes when the files move, artist and title stay
  , peArtist :: Text
  , peTitle :: Text
  , peVia :: Text -- clutter | import:discogs-list | import:want | import:albumsplayed | manual
  , peNote :: Maybe Text
  }
  deriving (Eq, Show, Generic)

peOptions :: Options
peOptions =
  defaultOptions
    { fieldLabelModifier = map toLower . drop 2
    , omitNothingFields = True
    }
instance ToJSON PlayEvent where
  toJSON = genericToJSON peOptions
  toEncoding = genericToEncoding peOptions
instance FromJSON PlayEvent where
  parseJSON = genericParseJSON peOptions
