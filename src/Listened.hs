{-# LANGUAGE LambdaCase #-}

--
-- clutter's own play log: one JSON-lines file per year, append-only
-- (see docs/listened-plan.md)
--
module Listened (
  PlayEvent (..),
  keySource,
  eventKey,
  playEvent,
  listenedDir,
  playFile,
  appendPlay,
  readPlayFile,
  readPlays,
  playedTags,
  listenedName,
  listenedYears,
  listenedRows,
  playAlbum,
) where

import Data.Aeson (eitherDecodeStrict, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time (ZonedTime, defaultTimeLocale, formatTime)
import Relude
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import System.IO (SeekMode (..), hFileSize, hPutStrLn, hSeek)
import Types (Album (..), AlbumKey (..), ArtistRef (..), PlayEvent (..), TagFolder (..))

keySource :: AlbumKey -> Text
keySource k = case k of
  D _ -> "discogs"
  T _ -> "tidal"
  A _ -> "apple"
  J _ -> "jellyfin"
  Q _ -> "qobuz"
  C _ -> "clutter" -- not a version: a play is logged against a version's key

-- the album a play was logged against, in its own catalogue
eventKey :: PlayEvent -> Maybe AlbumKey
eventKey e = case peSrc e of
  "discogs" -> D <$> peDiscogs e
  "tidal" -> T <$> peTidal e
  "apple" -> A <$> peApple e
  "jellyfin" -> J <$> peJellyfin e
  _ -> Nothing

-- the event for pressing "Album Played" now, with every id clutter knows
playEvent :: ZonedTime -> Album -> PlayEvent
playEvent now a =
  PlayEvent
    { peTs = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Ez" now
    , peSrc = keySource k
    , peDiscogs = case k of D i -> Just i; _ -> Nothing
    , peInst = case k of D _ | albumInst a /= 0 -> Just (albumInst a); _ -> Nothing
    , peTidal = readId =<< albumTidal a
    , peApple = readId =<< albumAMusic a
    , peApplelib = case k of A _ | isNothing (readId =<< albumAMusic a) -> albumLocation a; _ -> Nothing
    , peJellyfin = T.toLower <$> mfilter (not . T.null) (albumJellyfin a)
    , peArtist = albumArtist a
    , peTitle = albumTitle a
    , peVia = "clutter"
    , peNote = Nothing
    }
 where
  k = albumID a
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

--
-- read side: which album a play belongs to is the album layer's answer
-- (Collection.buildLayer, Layer.lyPlay; the plays go on the albums in
-- Collection.attachAlbumPlays)
--

-- played or not: a play in the log, or the Discogs field 7 mark (plays before the log
-- began, 2019); how often: the log
playedTags :: Int -> Int -> [Text]
playedTags n discogs = howOften <> ["played.early" | discogs > n]
 where
  howOften = case n of
    0 | discogs == 0 -> ["played.never"]
    0 -> ["played."]
    1 -> ["played.", "played.once"]
    2 -> ["played.", "played.twice"]
    _ -> ["played.", "played.often"]

-- "Listened" -> Just Nothing (every play), "Listened 2026" -> Just (Just "2026")
listenedName :: Text -> Maybe (Maybe Text)
listenedName ln = case words ln of
  ["Listened"] -> Just Nothing
  ["Listened", y] | T.length y == 4 && T.all (`elem` ['0' .. '9']) y -> Just (Just y)
  _ -> Nothing

-- the years that have plays, newest first
listenedYears :: [PlayEvent] -> [Text]
listenedYears = sortOn Down . ordNub . map (T.take 4 . peTs)

-- one row per play of the year (or all years), newest first, with the album it is
-- shown on: the album clutter knows, or one made up from the play itself
-- (a release no longer in the collection, a Tidal album no longer a favourite)
-- The album is its row's (resolve: the key of the row of the album the play counts for).
listenedRows :: Maybe Text -> [PlayEvent] -> (PlayEvent -> Maybe AlbumKey) -> Map AlbumKey Album -> [(PlayEvent, Maybe Album)]
listenedRows my es resolve am =
  [ (e, (`M.lookup` am) =<< resolve e)
  | e <- sortOn (Down . peTs) es
  , maybe True (`T.isPrefixOf` peTs e) my
  ]

-- an album made up from a play, for plays of albums clutter does not hold
-- (yet, or any more), under the key it was logged with
playAlbum :: PlayEvent -> Album
playAlbum e =
  Album
    { albumID = fromMaybe (D 0) (eventKey e)
    , albumInst = 0
    , albumTitle = peTitle e
    , albumArtist = peArtist e
    , albumReleased = ""
    , albumCover = ""
    , albumAdded = ""
    , albumFolder = case peSrc e of
        "tidal" -> fromEnum TTidal
        "apple" -> fromEnum TAMusic
        "jellyfin" -> fromEnum TJellyfin
        _ -> fromEnum TDiscogs
    , albumURL = case (peDiscogs e, peTidal e) of
        (Just d, _) -> "https://www.discogs.com/release/" <> show d
        (_, Just t) -> "https://listen.tidal.com/album/" <> show t
        _ -> ""
    , albumFormat = case peSrc e of
        "tidal" -> "Tidal"
        "apple" -> "AppleMusic"
        "jellyfin" -> "Jellyfin"
        _ -> ""
    , albumTidal = show <$> peTidal e
    , albumAMusic = show <$> peApple e
    , albumQobuz = Nothing
    , albumJellyfin = peJellyfin e
    , albumLocIdx = Nothing
    , albumLocation = Nothing
    , albumTags = []
    , albumRating = 0
    , albumPlays = 0
    , albumListenedDates = [peTs e]
    , albumFavoriteUntil = Nothing
    , albumWikiKeys = catMaybes [("t:" <>) . show <$> peTidal e, ("a:" <>) . show <$> peApple e]
    , albumWiki = Nothing
    , albumAllMusic = Nothing
    , albumArtists = [ArtistRef{arName = peArtist e, arKey = "", arWiki = Nothing, arAllMusic = Nothing}]
    , albumFiles = []
    }
