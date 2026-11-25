{-# LANGUAGE LambdaCase #-}

-- What is in the collection (docs/jellyfin-plan.md, "Membership").
--
-- clutter's own log, ~/Sync/clutter/collection.jsonl (CLUTTER_COLLECTION overrides): one
-- line per change, appended, never rewritten. For now it holds the keep mark:
--
--   {"ts":"2026-11-25T16:02:11-06:00","op":"keep","key":"T283130328","value":"yes",
--    "artist":"…","title":"…"}
--
-- value: "yes" (keep), "no" (not for me), "unset" (back to the rule). The last line for a
-- key counts.
--
-- And the links between versions: a file album (Jellyfin) that is the same album as a
-- Discogs release or a Tidal album:
--
--   {"ts":"…","op":"link","key":"D1365907","version":"J32aa…","via":"inventory:sure"}
--   {"ts":"…","op":"unlink","version":"J32aa…"}
--
-- A file album is a version of one album at most; the last line for it counts. The linked
-- album stands for the file album in the lists, shows it under "On the server", and
-- carries its storage.* tags. Lines with another "op" are left alone.
--
-- The rule: an album is in the collection if it is marked yes; or it is not marked and is
-- a record on a shelf (a Discogs release outside the stand-in folders) or has been played.
-- Tags, recomputed whenever the plays are: collection.in / collection.inbox (not judged
-- yet) / collection.out (marked no), and keep.yes / keep.no for the mark itself.
module Collection (
  Marks,
  Links,
  collectionFile,
  readCollection,
  appendMark,
  appendLink,
  attachLinks,
  masterKey,
  attachCollection,
  standInFolderTags,
) where

import Data.Aeson (Value, eitherDecodeStrict, encode, object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (ZonedTime, defaultTimeLocale, formatTime)
import Relude
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.IO (SeekMode (..), hFileSize, hPutStrLn, hSeek)
import Types (Album (..), AlbumKey (..), parseKey)

-- the keep mark per album: True = keep, False = not for me; no entry = not set
type Marks = Map AlbumKey Bool

collectionFile :: IO FilePath
collectionFile =
  lookupEnv "CLUTTER_COLLECTION" >>= \case
    Just f | not (null f) -> pure f
    _ -> do
      home <- fromMaybe "." <$> lookupEnv "HOME"
      pure $ home </> "Sync" </> "clutter" </> "collection.jsonl"

-- the Discogs folders whose releases are stand-ins for a stream or for files, not records
-- on a shelf, as folder tags: CLUTTER_STANDIN_FOLDERS (comma-separated folder names),
-- else Streaming and Files
standInFolderTags :: IO (Set Text)
standInFolderTags = do
  names <- maybe "Streaming,Files" toText <$> lookupEnv "CLUTTER_STANDIN_FOLDERS"
  pure . S.fromList $ [T.toCaseFold ("folder." <> T.strip n) | n <- T.splitOn "," names, not (T.null (T.strip n))]

-- file album (Jellyfin id) -> the album it is a version of
type Links = Map Text AlbumKey

data Event = Keep AlbumKey (Maybe Bool) | Link Text AlbumKey | Unlink Text

-- Nothing for a line with an "op" this version does not know
logLine :: Value -> Parser (Maybe Event)
logLine = withObject "collection event" $ \o -> do
  op <- o .: "op"
  case (op :: Text) of
    "keep" -> do
      k <- o .: "key"
      v <- o .: "value"
      case (parseKey k, v :: Text) of
        (Just key, "yes") -> pure (Just (Keep key (Just True)))
        (Just key, "no") -> pure (Just (Keep key (Just False)))
        (Just key, "unset") -> pure (Just (Keep key Nothing))
        _ -> fail "not a keep mark"
    "link" -> do
      k <- o .: "key"
      v <- o .: "version"
      case (parseKey k, parseKey v) of
        (Just (J _), _) -> fail "a file album cannot stand for another"
        (Just key, Just (J j)) -> pure (Just (Link j key))
        _ -> fail "not a link"
    "unlink" -> do
      v <- o .: "version"
      case parseKey v of
        Just (J j) -> pure (Just (Unlink j))
        _ -> fail "not an unlink"
    _ -> pure Nothing

-- the marks and links as the log leaves them; a missing file is none of either, an
-- undecodable line is skipped with a warning
readCollection :: FilePath -> IO (Marks, Links)
readCollection fn = do
  exists <- doesFileExist fn
  if not exists
    then pure (M.empty, M.empty)
    else do
      ls <- BS8.lines <$> readFileBS fn
      es <- fmap catMaybes . forM (zip [1 :: Int ..] ls) $ \(n, l) ->
        if BS8.all (`elem` [' ', '\t', '\r']) l
          then pure Nothing
          else case eitherDecodeStrict l >>= parseEither logLine of
            Right e -> pure e
            Left err -> do
              hPutStrLn stderr $ "WARNING: skipping " <> fn <> ":" <> show n <> ": " <> err
              pure Nothing
      let step (ms, lk) = \case
            Keep k v -> (M.alter (const v) k ms, lk)
            Link j k -> (ms, M.insert j k lk)
            Unlink j -> (ms, M.delete j lk)
          (marks, links) = foldl' step (M.empty, M.empty) es
      hPutStrLn stderr $ "read " <> show (M.size marks) <> " keep marks and " <> show (M.size links) <> " links from " <> fn
      pure (marks, links)

-- append one line; never rewrites what is already there
appendLine :: FilePath -> Value -> IO ()
appendLine fn line = do
  createDirectoryIfMissing True (takeDirectory fn)
  -- a torn last line from a crash must not swallow this event too
  torn <- endsTorn
  withFile fn AppendMode $ \h ->
    BL.hPut h $ (if torn then "\n" else "") <> encode line <> "\n"
 where
  endsTorn = do
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

stamp :: ZonedTime -> Text
stamp = toText . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Ez"

-- append one keep mark
appendMark :: FilePath -> ZonedTime -> Album -> Maybe Bool -> IO ()
appendMark fn now a mark =
  appendLine fn $
    object
      [ "ts" .= stamp now
      , "op" .= ("keep" :: Text)
      , "key" .= albumID a
      , "value" .= (case mark of Just True -> "yes"; Just False -> "no"; Nothing -> "unset" :: Text)
      , "artist" .= albumArtist a
      , "title" .= albumTitle a
      ]

-- append a link (Just the album the file album is a version of) or an unlink (Nothing)
appendLink :: FilePath -> ZonedTime -> Text -> Maybe Album -> IO ()
appendLink fn now j = \case
  Just a ->
    appendLine fn $
      object
        [ "ts" .= stamp now
        , "op" .= ("link" :: Text)
        , "key" .= albumID a
        , "version" .= J j
        , "via" .= ("clutter" :: Text)
        , "artist" .= albumArtist a
        , "title" .= albumTitle a
        ]
  Nothing -> appendLine fn $ object ["ts" .= stamp now, "op" .= ("unlink" :: Text), "version" .= J j]

-- put the linked file albums on the albums they are versions of: albumFiles (after those
-- the Discogs Location field names), albumJellyfin (the first), and the tags
-- provider.jellyfin and storage.* of those file albums. A link to an album or from a file
-- album clutter does not hold is left out. Idempotent; an unlinked file has to be taken
-- off albumFiles by the caller (Env.envSetLink), since a Location id looks the same.
-- Runs before attachPlays (a file album's plays are its album's) and attachLength.
--
-- Releases of one Discogs master (the vinyl and the CD of an album) share the storage
-- tags: the files linked to one of them are a version of the album, whichever release
-- holds the link. albumFiles stays with that release (stand-in, plays); the album page
-- shows the files on the others too (App.filesOf).
attachLinks :: Links -> Map AlbumKey Album -> Map AlbumKey Album
attachLinks links am = M.mapWithKey attach am
 where
  byAlbum = M.fromListWith (flip (<>)) [(k, [j]) | (j, k) <- M.toList links, M.member (J j) am]
  ownFiles k a = case k of
    J _ -> []
    _ -> ordNub (albumFiles a <> M.findWithDefault [] k byAlbum)
  byMaster = M.fromListWith (<>) [(m, fs) | (k, a) <- M.toList am, Just m <- [masterKey a], let fs = ownFiles k a, not (null fs)]
  storageOf j = [t | Just f <- [M.lookup (J j) am], t <- albumTags f, "storage." `T.isPrefixOf` t]
  attach k a = case k of
    J _ -> a
    _ ->
      let files = ownFiles k a
          shared = maybe [] (\m -> M.findWithDefault [] m byMaster) (masterKey a)
          own = filter (\t -> not ("storage." `T.isPrefixOf` t)) (albumTags a)
          fileTags = ordNub (concatMap storageOf (files <> shared))
          tags = own <> ["provider.jellyfin" | "provider.jellyfin" `notElem` own, not (null fileTags)] <> fileTags
       in if null files
            then a{albumTags = tags}
            else a{albumFiles = files, albumJellyfin = viaNonEmpty head files, albumTags = tags}

-- the Discogs master of a release, as its Wikidata store key ("dm:<id>"): releases with
-- the same one are versions of one album
masterKey :: Album -> Maybe Text
masterKey a = case albumID a of
  D _ -> find ("dm:" `T.isPrefixOf`) (albumWikiKeys a)
  _ -> Nothing

-- set the collection.* and keep.* tags on every album. Needs the played.* tags, so it
-- runs after Listened.attachPlays. Idempotent.
attachCollection :: Set Text -> Marks -> Map AlbumKey Album -> Map AlbumKey Album
attachCollection standInTags marks = M.map $ \a ->
  let tags = filter (\t -> not ("collection." `T.isPrefixOf` t || "keep." `T.isPrefixOf` t)) (albumTags a)
      onShelf = case albumID a of
        D _ -> not (any (`S.member` standInTags) tags)
        _ -> False
      played = "played.never" `notElem` tags
      new = case M.lookup (albumID a) marks of
        Just True -> ["keep.yes", "collection.in"]
        Just False -> ["keep.no", "collection.out"]
        Nothing -> [if onShelf || played then "collection.in" else "collection.inbox"]
   in a{albumTags = tags <> new}
