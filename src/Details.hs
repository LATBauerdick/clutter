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
) where

import Data.Aeson (FromJSON (..), Value, eitherDecode, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Relude
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO (hPutStrLn)
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
  }

data DetailStore = DetailStore
  { dsDiscogs :: Map Int DFacts -- by Discogs release id
  , dsTidal :: Map Int TFacts -- by Tidal album id
  }

emptyDetailStore :: DetailStore
emptyDetailStore = DetailStore M.empty M.empty

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
readDetailStore = DetailStore <$> readDiscogs <*> readTidal

factsFor :: DetailStore -> Album -> AlbumFacts
factsFor ds a =
  AlbumFacts
    { afLength = tf >>= tfLength
    , afTracks = tf >>= tfTracks
    , afQuality = tf >>= tfQuality
    , afLabel = (df >>= dfLabel) <|> (tf >>= tfCopyright)
    , afReleased = tf >>= tfReleased
    , afKind = tf >>= tfKind
    , afExplicit = maybe False tfExplicit tf
    , afStyles = maybe [] dfStyles df
    }
 where
  tidalId = case albumID a of
    T n -> Just n
    _ -> albumTidal a >>= readMaybe . toString . T.strip
  tf = tidalId >>= (`M.lookup` dsTidal ds)
  df = case albumID a of
    D n -> M.lookup n (dsDiscogs ds)
    _ -> Nothing
