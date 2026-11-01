{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

--
-- clutter's own play log: one JSON-lines file per year, append-only
-- (see docs/listened-plan.md)
--
module Listened (
  AlbumKey (..),
  PlayEvent (..),
  albumKey,
  keySource,
  playEvent,
  listenedDir,
  playFile,
  appendPlay,
  readPlayFile,
  readPlays,
) where

import Data.Aeson (FromJSON (..), Options (..), ToJSON (..), defaultOptions, eitherDecodeStrict, encode, genericParseJSON, genericToEncoding, genericToJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import qualified Data.Text as T
import Data.Time (ZonedTime, defaultTimeLocale, formatTime)
import Relude
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import System.IO (SeekMode (..), hFileSize, hPutStrLn, hSeek)
import Types (Album (..), TagFolder (..))

-- album identity, typed by catalogue: the same Int means different
-- releases in Discogs, Tidal and Apple Music
data AlbumKey = KDiscogs Int | KTidal Int | KApple Int
  deriving (Eq, Ord, Show)

-- Album.albumID holds the Tidal (Apple) id for Tidal (Apple Music) albums,
-- which are marked by their pseudo-folder; everything else is a Discogs release.
-- An Apple album without a catalog id has a hash of its library id as albumID.
albumKey :: Album -> AlbumKey
albumKey a
  | albumFolder a == fromEnum TTidal = KTidal (albumID a)
  | albumFolder a == fromEnum TAMusic = KApple (albumID a)
  | otherwise = KDiscogs (albumID a)

keySource :: AlbumKey -> Text
keySource k = case k of
  KDiscogs _ -> "discogs"
  KTidal _ -> "tidal"
  KApple _ -> "apple"

data PlayEvent = PlayEvent
  { peTs :: Text -- local time with UTC offset, or a bare date for imported date-only plays
  , peSrc :: Text -- discogs|tidal|apple|qobuz|jellyfin
  , peDiscogs :: Maybe Int
  , peInst :: Maybe Int
  , peTidal :: Maybe Int
  , peApple :: Maybe Int
  , peApplelib :: Maybe Text -- Apple Music library id, for albums without a catalog id
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

-- the event for pressing "Album Played" now, with every id clutter knows
playEvent :: ZonedTime -> Album -> PlayEvent
playEvent now a =
  PlayEvent
    { peTs = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Ez" now
    , peSrc = keySource k
    , peDiscogs = case k of KDiscogs i -> Just i; _ -> Nothing
    , peInst = case k of KDiscogs _ | albumInst a /= 0 -> Just (albumInst a); _ -> Nothing
    , peTidal = readId =<< albumTidal a
    , peApple = readId =<< albumAMusic a
    , peApplelib = case k of KApple _ | isNothing (readId =<< albumAMusic a) -> albumLocation a; _ -> Nothing
    , peArtist = albumArtist a
    , peTitle = albumTitle a
    , peVia = "clutter"
    , peNote = Nothing
    }
 where
  k = albumKey a
  readId :: Text -> Maybe Int
  readId = readMaybe . toString

-- CLUTTER_LISTENED, else ~/Sync/clutter/listened (synced and snapshotted, unlike cache/)
listenedDir :: IO FilePath
listenedDir =
  lookupEnv "CLUTTER_LISTENED" >>= \case
    Just d | not (null d) -> pure d
    _ -> do
      home <- fromMaybe "." <$> lookupEnv "HOME"
      pure $ home </> "Sync" </> "clutter" </> "listened"

-- listened/YYYY.jsonl, by the year of the event's own timestamp
playFile :: FilePath -> PlayEvent -> FilePath
playFile dir e = dir </> toString (T.take 4 (peTs e)) <> ".jsonl"

-- append one line; never rewrites what is already there
appendPlay :: FilePath -> PlayEvent -> IO FilePath
appendPlay dir e = do
  createDirectoryIfMissing True dir
  let fn = playFile dir e
  -- a torn last line from a crash must not swallow this event too
  torn <- endsTorn fn
  withFile fn AppendMode $ \h ->
    BL.hPut h $ (if torn then "\n" else "") <> encode e <> "\n"
  pure fn
 where
  endsTorn fn = do
    exists <- doesFileExist fn
    if not exists
      then pure False
      else withFile fn ReadMode $ \h -> do
        size <- hFileSize h
        if size == 0
          then pure False
          else do
            hSeek h AbsoluteSeek (size - 1)
            (/= "\n") <$> BS.hGet h 1

-- all events of one file; undecodable lines (a torn last line) are skipped with a warning
readPlayFile :: FilePath -> IO [PlayEvent]
readPlayFile fn = do
  ls <- BS8.lines <$> readFileBS fn
  fmap catMaybes . forM (zip [1 :: Int ..] ls) $ \(n, l) ->
    if BS8.all (`elem` [' ', '\t', '\r']) l
      then pure Nothing
      else case eitherDecodeStrict l of
        Right e -> pure (Just e)
        Left err -> do
          hPutStrLn stderr $ "WARNING: skipping " <> fn <> ":" <> show n <> ": " <> err
          pure Nothing

-- every *.jsonl in the directory, in file name (= year) order
readPlays :: FilePath -> IO [PlayEvent]
readPlays dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      fns <- sort . filter ((== ".jsonl") . takeExtension) <$> listDirectory dir
      concat <$> mapM (readPlayFile . (dir </>)) fns
