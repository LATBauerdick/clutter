{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- Per-album facts that are on disk but not in the Album record: length, track count, audio
-- quality, label, full release date, kind, explicit flag, Discogs styles. Read once at server
-- start from the Discogs collection cache (cache/draw<N>.json) and the Tidal album store
-- (~/Sync/clutter/tidal-albums.json, CLUTTER_TIDAL_ALBUMS overrides). A missing or unreadable
-- file means no facts from it, no error; a malformed entry is skipped, not the whole file.
-- Phase 1 of docs/album-page-plan.md; the facts go out with /api/albumxq.
module Details (
  DetailStore,
  AlbumFacts (..),
  emptyDetailStore,
  readDetailStore,
  factsFor,
  attachLength,
) where

import Data.Aeson (FromJSON (..), Value, eitherDecode, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Relude
import Control.Exception (try)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.IO (hPutStrLn)
import Tracks (cacheDir)
import Types (Album (..), AlbumFacts (..), AlbumKey (..))
import Wikidata (displayName)

-- Discogs release facts
data DFacts = DFacts
  { dfLabel :: Maybe Text
  , dfStyles :: [Text]
  }

-- Tidal album facts
data TFacts = TFacts
  { tfLength :: Maybe Int
  , tfTracks :: Maybe Int
  , tfQuality :: Maybe Text
  , tfCopyright :: Maybe Text
  , tfReleased :: Maybe Text
  , tfKind :: Maybe Text
  , tfExplicit :: Bool
  , tfGone :: Maybe Text
  }

data DetailStore = DetailStore
  { dsDiscogs :: Map Int DFacts -- by Discogs release id
  , dsTidal :: Map Int TFacts -- by Tidal album id
  , dsCached :: Map Int Int -- by Discogs release id: seconds, summed from its cached tracks
  , dsLengths :: Map Int (Maybe Int, Maybe Int) -- by Discogs release id: seconds, tracks (album-lengths.json)
  }

emptyDetailStore :: DetailStore
emptyDetailStore = DetailStore M.empty M.empty M.empty M.empty

-- Discogs ---------------------------------------------------------------------------------

-- the "releases" array, kept as raw values so one odd release cannot spoil the file
newtype DPage = DPage [Value]
instance FromJSON DPage where
  parseJSON = withObject "discogs page" $ \o -> DPage <$> o .:? "releases" .!= []

data DLabel = DLabel {dlName :: Maybe Text, dlCatno :: Maybe Text}
instance FromJSON DLabel where
  parseJSON = withObject "discogs label" $ \o -> DLabel <$> o .:? "name" <*> o .:? "catno"

parseRelease :: Value -> Parser (Int, DFacts)
parseRelease = withObject "discogs release" $ \o -> do
  rid <- o .: "id"
  bi <- o .:? "basic_information"
  (labels, styles) <- case bi of
    Nothing -> pure ([], [])
    Just b ->
      withObject
        "basic_information"
        (\bo -> (,) <$> bo .:? "labels" .!= [] <*> bo .:? "styles" .!= [])
        b
  pure (rid, DFacts (viaNonEmpty head labels >>= labelText) styles)

-- "name · catno", just "name" without a catalogue number
labelText :: DLabel -> Maybe Text
labelText l = case T.strip . displayName <$> dlName l of
  Just n
    | not (T.null n) -> Just $ case T.strip <$> dlCatno l of
        Just c | not (T.null c), T.toLower c /= "none" -> n <> " · " <> c
        _ -> n
  _ -> Nothing

readDiscogs :: IO (Map Int DFacts)
readDiscogs = go 1 M.empty
 where
  go :: Int -> Map Int DFacts -> IO (Map Int DFacts)
  go n acc = do
    let fn = "cache" </> ("draw" <> show n <> ".json")
    ok <- doesFileExist fn
    if not ok
      then do
        when (n == 1) $ hPutStrLn stderr ("no Discogs cache at " <> fn <> " -- no labels/styles")
        when (n > 1) $
          hPutStrLn stderr $
            "read Discogs details from " <> show (n - 1) <> " cache files: " <> show (M.size acc) <> " releases"
        pure acc
      else
        eitherDecode <$> readFileLBS fn >>= \case
          Left err -> hPutStrLn stderr ("Discogs cache " <> fn <> ": " <> err) >> go (n + 1) acc
          Right (DPage vs) ->
            -- left-biased: the first occurrence of a release wins
            go (n + 1) (M.union acc (M.fromList (reverse (mapMaybe (parseMaybe parseRelease) vs))))

-- Tidal -----------------------------------------------------------------------------------

-- the "albums" object, entries kept raw so an entry without "album" is just skipped
newtype TStore = TStore (Map Text Value)
instance FromJSON TStore where
  parseJSON = withObject "tidal store" $ \o -> TStore <$> o .:? "albums" .!= M.empty

newtype TMeta = TMeta [Text]
instance FromJSON TMeta where
  parseJSON = withObject "mediaMetadata" $ \o -> TMeta <$> o .:? "tags" .!= []

parseTidal :: Value -> Parser TFacts
parseTidal = withObject "tidal entry" $ \e -> do
  a <- e .: "album"
  gone <- e .:? "gone"
  withObject
    "tidal album"
    ( \o -> do
        tags <- maybe [] (\(TMeta ts) -> ts) <$> o .:? "mediaMetadata"
        aq <- o .:? "audioQuality"
        typ <- o .:? "type"
        TFacts
          <$> o .:? "duration"
          <*> o .:? "numberOfTracks"
          <*> pure (qualityText (if null tags then maybeToList aq else tags))
          <*> (mfilter (not . T.null) <$> o .:? "copyright")
          <*> (mfilter (not . T.null) <$> o .:? "releaseDate")
          <*> pure (typ >>= kindText)
          <*> o .:? "explicit" .!= False
          <*> pure gone
    )
    a

qualityText :: [Text] -> Maybe Text
qualityText tags = case (base, atmos) of
  (Just b, True) -> Just (b <> " · Dolby Atmos")
  (Just b, False) -> Just b
  (Nothing, True) -> Just "Dolby Atmos"
  (Nothing, False) -> Nothing
 where
  base
    | "HIRES_LOSSLESS" `elem` tags = Just "Hi-Res lossless"
    | "LOSSLESS" `elem` tags = Just "Lossless"
    | otherwise = Nothing
  atmos = "DOLBY_ATMOS" `elem` tags

kindText :: Text -> Maybe Text
kindText = \case
  "EP" -> Just "EP"
  "SINGLE" -> Just "Single"
  _ -> Nothing

tidalStorePath :: IO FilePath
tidalStorePath =
  lookupEnv "CLUTTER_TIDAL_ALBUMS" >>= \case
    Just f | not (null f) -> pure f
    _ -> do
      home <- fromMaybe "." <$> lookupEnv "HOME"
      pure $ home </> "Sync" </> "clutter" </> "tidal-albums.json"

readTidal :: IO (Map Int TFacts)
readTidal = do
  fn <- tidalStorePath
  ok <- doesFileExist fn
  if not ok
    then hPutStrLn stderr ("no Tidal album store at " <> fn <> " -- no Tidal details") $> M.empty
    else
      eitherDecode <$> readFileLBS fn >>= \case
        Left err -> hPutStrLn stderr ("Tidal album store " <> fn <> ": " <> err) $> M.empty
        Right (TStore m) -> do
          let facts =
                M.fromList
                  [ (i, f)
                  | (k, v) <- M.toList m
                  , Just i <- [readMaybe (toString k)]
                  , Just f <- [parseMaybe parseTidal v]
                  ]
          hPutStrLn stderr $
            "read Tidal details from " <> fn <> ": " <> show (M.size facts) <> " of "
              <> show (M.size m) <> " albums"
          pure facts

-- store and lookup ------------------------------------------------------------------------

readDetailStore :: IO DetailStore
readDetailStore = DetailStore <$> readDiscogs <*> readTidal <*> readCached <*> readLengths

-- Playing times of the Discogs releases that the Tidal album store does not cover, looked up
-- by scripts/update-lengths.py: ~/Sync/clutter/album-lengths.json (CLUTTER_ALBUM_LENGTHS
-- overrides), {"releases": {"<id>": {"secs": …, "tracks": …}}}; secs is null where Discogs
-- has no durations. A missing or unreadable store means no lengths from it, no error.
readLengths :: IO (Map Int (Maybe Int, Maybe Int))
readLengths = do
  fn <-
    lookupEnv "CLUTTER_ALBUM_LENGTHS" >>= \case
      Just f | not (null f) -> pure f
      _ -> do
        home <- fromMaybe "." <$> lookupEnv "HOME"
        pure $ home </> "Sync" </> "clutter" </> "album-lengths.json"
  ok <- doesFileExist fn
  if not ok
    then hPutStrLn stderr ("no album lengths at " <> fn <> " -- run scripts/update-lengths.py") $> M.empty
    else
      eitherDecode <$> readFileLBS fn >>= \case
        Left err -> hPutStrLn stderr ("album lengths " <> fn <> ": " <> err) $> M.empty
        Right v -> do
          let m =
                M.fromList
                  [ (i, e)
                  | (k, x) <- maybe [] M.toList (parseMaybe releases v)
                  , Just i <- [readMaybe (toString k)]
                  , Just e <- [parseMaybe entry x]
                  ]
          hPutStrLn stderr $
            "read album lengths from " <> fn <> ": " <> show (M.size (M.filter (isJust . fst) m))
              <> " of " <> show (M.size m) <> " releases have one"
          pure m
 where
  releases :: Value -> Parser (Map Text Value)
  releases = withObject "album lengths" (\o -> o .:? "releases" .!= M.empty)
  entry :: Value -> Parser (Maybe Int, Maybe Int)
  entry = withObject "length" $ \o -> (,) <$> (mfilter (> 0) <$> o .:? "secs") <*> o .:? "tracks"

-- The length of a Discogs release that has no Tidal album to take it from: the sum of its
-- track durations, where the page's details cache (Tracks, album-details/D<id>.json) has the
-- release and Discogs gave a duration for every track. Only releases opened once are there.
readCached :: IO (Map Int Int)
readCached = do
  dir <- cacheDir
  ok <- doesDirectoryExist dir
  fns <- if ok then listDirectory dir else pure []
  M.fromList . catMaybes <$> mapM (entry dir) fns
 where
  entry :: FilePath -> FilePath -> IO (Maybe (Int, Int))
  entry dir fn = case T.stripSuffix ".json" (toText fn) >>= T.stripPrefix "D" >>= readMaybe . toString of
    Nothing -> pure Nothing -- other keys, and the "-tidal" versions
    Just n -> do
      r <- try (readFileLBS (dir </> fn)) :: IO (Either SomeException LByteString)
      pure $ case r of
        Right b | Just s <- rightToMaybe (eitherDecode b) >>= parseMaybe total -> Just (n, s)
        _ -> Nothing
  total :: Value -> Parser Int
  total = withObject "album details" $ \o -> do
    ts <- o .:? "atTracks" .!= []
    secs <- mapM (withObject "track" (.:? "tkSecs")) (ts :: [Value])
    case sequence (secs :: [Maybe Int]) of
      Just ss | sum ss > 0 -> pure (sum ss)
      _ -> fail "no durations"

-- length.* tags, so that the focus can pick albums by playing time: length.<30min, <40min,
-- <50min, <60min, <70min (an album of 25 minutes has all five), length.>70min, or
-- length.unknown. "50 minutes and more" is the focus "without length.<50min". Idempotent.
attachLength :: (Album -> AlbumFacts) -> Map k Album -> Map k Album
attachLength facts = M.map $ \a ->
  a{albumTags = filter (not . ("length." `T.isPrefixOf`)) (albumTags a) <> lengthTags (afLength (facts a))}

lengthTags :: Maybe Int -> [Text]
lengthTags = \case
  Just s
    | s > 0 ->
        ["length.<" <> show m <> "min" | m <- [30, 40, 50, 60, 70 :: Int], s < m * 60]
          <> ["length.>70min" | s >= 70 * 60]
  _ -> ["length.unknown"]

factsFor :: DetailStore -> Album -> AlbumFacts
factsFor ds a =
  AlbumFacts
    { afLength = (tf >>= tfLength) <|> (looked >>= fst) <|> cached
    , afTracks = (tf >>= tfTracks) <|> (looked >>= snd)
    , afQuality = tf >>= tfQuality
    , afLabel = (df >>= dfLabel) <|> (tf >>= tfCopyright)
    , afReleased = tf >>= tfReleased
    , afKind = tf >>= tfKind
    , afExplicit = maybe False tfExplicit tf
    , afStyles = maybe [] dfStyles df
    , afGone = tf >>= tfGone
    }
 where
  tidalId = case albumID a of
    T n -> Just n
    _ -> albumTidal a >>= readMaybe . toString . T.strip
  tf = tidalId >>= (`M.lookup` dsTidal ds)
  df = case albumID a of
    D n -> M.lookup n (dsDiscogs ds)
    _ -> Nothing
  cached = case albumID a of
    D n -> M.lookup n (dsCached ds)
    _ -> Nothing
  looked = case albumID a of
    D n -> M.lookup n (dsLengths ds)
    _ -> Nothing
