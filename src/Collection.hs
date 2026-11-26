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
-- carries its storage.* tags.
--
-- The same two lines with a Tidal, Apple Music or Qobuz album as "version" ("T5201052",
-- "A399275146", "Q0060254728436") give a Discogs release its stream: one of each kind per
-- release, the last line counts; an unlink names the release too ("key"), since several
-- releases may have the same stream. This log is the only place these links are kept: the
-- ids once written into the Discogs Location field were brought over by
-- scripts/import-location-links.py (via "discogs-location") and are not read any more.
-- Lines with another "op" are left alone.
--
-- A file album that the review of the inventory's unsure matches passed over (none of the
-- albums offered is it) gets a "skip" line, so that it is not offered again; linking it by
-- hand later still works:
--
--   {"ts":"…","op":"skip","version":"J32aa…","artist":"…","title":"…"}
--
-- One suggestion under an album page's "Same album as…" that is not the same album (× on
-- the page): that album is not suggested for this one again. "undo" (↺) brings it back;
-- the last line for the pair counts.
--
--   {"ts":"…","op":"notsame","version":"J097b…","key":"D1234","artist":"…","title":"…"}
--   {"ts":"…","op":"notsame","version":"J097b…","key":"D1234","undo":true}
--
-- Jellyfin gives a folder that was moved or renamed a new id. When a full update from the
-- server (scripts/update-jellyfin-albums.py) finds that a file album clutter refers to has
-- gone and one new album is the same one, it notes the move:
--
--   {"ts":"…","op":"moved","from":"J32aa…","to":"J7f01…","via":"update-jellyfin-albums",
--    "artist":"…","title":"…","from_path":"…","to_path":"…"}
--
-- Every file album id in the log, and in the play log (Env), is read through these lines:
-- links, keep marks, skips and plays of the old id are the new one's. Nothing is rewritten.
-- A later move of the new id is followed too; a move back takes the earlier one off.
--
-- The rule: an album is in the collection if it is marked yes; or it is not marked and is
-- a record on a shelf (a Discogs release outside the stand-in folders) or has been played.
-- Tags, recomputed whenever the plays are: collection.in / collection.inbox (not judged
-- yet) / collection.out (marked no), and keep.yes / keep.no for the mark itself.
module Collection (
  Marks,
  Links,
  Streams,
  Moves,
  followMove,
  movedKey,
  sameKind,
  parseStreamRef,
  appendStream,
  collectionFile,
  readCollection,
  appendMark,
  appendLink,
  appendSkip,
  appendNotSame,
  notSame,
  reviewedFiles,
  readMoves,
  attachLinks,
  masterKey,
  attachCollection,
  standInFolderTags,
) where

import Data.Aeson (Value, eitherDecodeStrict, encode, object, withObject, (.!=), (.:), (.:?), (.=))
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

-- a file album's old Jellyfin id -> the id its folder has since it moved
type Moves = Map Text Text

-- the id a file album has now: the moves followed to the end (a loop stops where it closes)
followMove :: Moves -> Text -> Text
followMove mv = go S.empty
 where
  go seen j = case M.lookup j mv of
    Just j' | not (S.member j' seen), j' /= j -> go (S.insert j seen) j'
    _ -> j

movedKey :: Moves -> AlbumKey -> AlbumKey
movedKey mv = \case
  J j -> J (followMove mv j)
  k -> k

-- Discogs release (or a file album that stands for itself) -> its streams: one Tidal,
-- one Apple Music, one Qobuz album at most.
-- Several releases may have the same stream (the vinyl and the CD of an album).
type Streams = Map AlbumKey [AlbumKey]

data Event
  = Keep AlbumKey (Maybe Bool)
  | Link Text AlbumKey
  | Unlink Text
  | LinkStream AlbumKey AlbumKey -- stream, release
  | UnlinkStream AlbumKey (Maybe AlbumKey) -- stream, release (every release, if none is named)
  | Moved Text Text -- old Jellyfin id, new one

-- what can have a stream: a Discogs release, a file album
holds :: AlbumKey -> Bool
holds = \case D _ -> True; J _ -> True; _ -> False

isStream :: AlbumKey -> Bool
isStream = \case T _ -> True; A _ -> True; Q _ -> True; _ -> False

-- both Tidal albums, both Apple albums, or both Qobuz albums
sameKind :: AlbumKey -> AlbumKey -> Bool
sameKind (T _) (T _) = True
sameKind (A _) (A _) = True
sameKind (Q _) (Q _) = True
sameKind _ _ = False

-- one stream of a kind per release: a new one takes the place of the old
insertStream :: AlbumKey -> AlbumKey -> Streams -> Streams
insertStream s k = M.alter (\ms -> Just (s : filter (not . sameKind s) (fromMaybe [] ms))) k

deleteStream :: AlbumKey -> Maybe AlbumKey -> Streams -> Streams
deleteStream s mk = M.filter (not . null) . maybe (M.map (filter (/= s))) (M.adjust (filter (/= s))) mk

-- What LATB pastes to name a stream album: "T5201052", "A399275146", or a share link --
-- https://tidal.com/album/5201052?u, https://listen.tidal.com/album/5201052,
-- tidal://album/5201052, https://music.apple.com/us/album/before-the-flood/399275146?i=…
parseStreamRef :: Text -> Maybe AlbumKey
parseStreamRef raw
  | Just k@(T _) <- parseKey t = Just k
  | Just k@(A _) <- parseKey t = Just k
  | Just k@(Q _) <- parseKey t = Just k
  | "qobuz.com" `T.isInfixOf` low = viaNonEmpty last (filter (not . T.null) (T.splitOn "/" path)) >>= parseKey . ("Q" <>)
  | "tidal" `T.isInfixOf` low = T <$> (after "album/" >>= number)
  | "music.apple.com" `T.isInfixOf` low || "music://" `T.isPrefixOf` low || "itunes.apple.com" `T.isInfixOf` low =
      A <$> (viaNonEmpty last (filter isId (T.splitOn "/" path)) >>= number . T.dropWhile (not . isDigit))
  | otherwise = Nothing
 where
  t = T.strip raw
  low = T.toLower t
  path = T.takeWhile (\c -> c /= '?' && c /= '#') t
  after marker = case T.breakOnEnd marker path of
    (pre, rest) | not (T.null pre) -> Just rest
    _ -> Nothing
  isDigit c = c >= '0' && c <= '9'
  number x = let ds = T.takeWhile isDigit x in if T.null ds then Nothing else readMaybe (toString ds)
  -- an Apple album id is the last path piece: digits, or "id" + digits
  isId p = let q = fromMaybe p (T.stripPrefix "id" p) in not (T.null q) && T.all isDigit q

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
        -- a stream of a release, or of a file album that stands for itself
        (Just key, Just st) | isStream st, holds key -> pure (Just (LinkStream st key))
        (Just (J _), _) -> fail "a file album cannot stand for another"
        (Just key, Just (J j)) -> pure (Just (Link j key))
        _ -> fail "not a link"
    "unlink" -> do
      v <- o .: "version"
      case parseKey v of
        Just (J j) -> pure (Just (Unlink j))
        Just st | isStream st -> do
          mk <- o .:? "key"
          pure (Just (UnlinkStream st (mk >>= parseKey)))
        _ -> fail "not an unlink"
    "moved" -> do
      f <- o .: "from"
      t <- o .: "to"
      case (parseKey f, parseKey t) of
        (Just (J a), Just (J b)) -> pure (Just (Moved (T.toLower a) (T.toLower b)))
        _ -> fail "not a move of a file album"
    _ -> pure Nothing

-- the moves in the order of the log: a move to an id takes off a move from it (a folder
-- moved back), so that following them ends
foldMoves :: [(Text, Text)] -> Moves
foldMoves = foldl' (\mv (a, b) -> M.insert a b (M.delete b mv)) M.empty

-- the marks and links as the log leaves them, with every file album under the id it has
-- now, and the moves; a missing file is none of them, an undecodable line is skipped with
-- a warning
readCollection :: FilePath -> IO (Marks, Links, Streams, Moves)
readCollection fn = do
  exists <- doesFileExist fn
  if not exists
    then pure (M.empty, M.empty, M.empty, M.empty)
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
      let moves = foldMoves [(a, b) | Moved a b <- es]
          j = followMove moves
          k = movedKey moves
          step (ms, lk, st) = \case
            Keep a v -> (M.alter (const v) (k a) ms, lk, st)
            Link f a -> (ms, M.insert (j f) a lk, st)
            Unlink f -> (ms, M.delete (j f) lk, st)
            LinkStream s a -> (ms, lk, insertStream s (k a) st)
            UnlinkStream s ma -> (ms, lk, deleteStream s (k <$> ma) st)
            Moved _ _ -> (ms, lk, st)
          (marks, links, streams) = foldl' step (M.empty, M.empty, M.empty) es
      hPutStrLn stderr $ "read " <> show (M.size marks) <> " keep marks and " <> show (M.size links) <> " file links, " <> show (sum (length <$> streams)) <> " stream links, " <> show (M.size moves) <> " moved file albums from " <> fn
      pure (marks, links, streams, moves)

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

-- append a skip: the review of the unsure matches found none of the albums offered to be
-- this file album (the album given is the file album itself)
appendSkip :: FilePath -> ZonedTime -> Album -> IO ()
appendSkip fn now a = case albumID a of
  J j ->
    appendLine fn $
      object
        [ "ts" .= stamp now
        , "op" .= ("skip" :: Text)
        , "version" .= J j
        , "artist" .= albumArtist a
        , "title" .= albumTitle a
        ]
  _ -> pure ()

-- the moves alone, for a reader that has the old ids (the inventory's TSV)
readMoves :: FilePath -> IO Moves
readMoves fn = do
  exists <- doesFileExist fn
  if not exists
    then pure M.empty
    else do
      ls <- BS8.lines <$> readFileBS fn
      pure $ foldMoves [(a, b) | l <- ls, "\"moved\"" `BS.isInfixOf` l, Right (Just (Moved a b)) <- [eitherDecodeStrict l >>= parseEither logLine]]

-- append that a suggestion is not the same album as this one (True), or take that back
appendNotSame :: FilePath -> ZonedTime -> AlbumKey -> Album -> Bool -> IO ()
appendNotSame fn now this cand excluded =
  appendLine fn . object $
    [ "ts" .= stamp now
    , "op" .= ("notsame" :: Text)
    , "version" .= this
    , "key" .= albumID cand
    ]
      <> (if excluded then ["artist" .= albumArtist cand, "title" .= albumTitle cand] else ["undo" .= True])

-- the suggestions excluded for each album (both keys as they are now: moved file albums
-- under their new ids), in the order they were excluded
notSame :: FilePath -> IO (Map AlbumKey [AlbumKey])
notSame fn = do
  exists <- doesFileExist fn
  if not exists
    then pure M.empty
    else do
      ls <- BS8.lines <$> readFileBS fn
      let vs = [v | l <- ls, "\"notsame\"" `BS.isInfixOf` l || "\"moved\"" `BS.isInfixOf` l, Right v <- [eitherDecodeStrict l]]
          moves = foldMoves [(T.toLower a, T.toLower b) | v <- vs, Right (Just (Moved a b)) <- [parseEither logLine v]]
          k = movedKey moves
          step m (this, cand, undo) = M.alter (\cs -> let cs' = filter (/= cand) (fromMaybe [] cs) in if undo then mfilter (not . null) (Just cs') else Just (cs' <> [cand])) this m
      pure $
        foldl'
          step
          M.empty
          [ (k this, k cand, undo)
          | v <- vs
          , Right ("notsame", Just tv, Just cv, undo) <- [parseEither pairOf v]
          , Just this <- [parseKey tv]
          , Just cand <- [parseKey cv]
          ]
 where
  pairOf :: Value -> Parser (Text, Maybe Text, Maybe Text, Bool)
  pairOf = withObject "collection event" $ \o -> (,,,) <$> o .: "op" <*> o .:? "version" <*> o .:? "key" <*> o .:? "undo" .!= False

-- the file albums (Jellyfin ids, as they are now) that the log has a link, unlink or skip
-- line for: decided once, by hand or by the import, and not offered for review again
reviewedFiles :: FilePath -> IO (Set Text)
reviewedFiles fn = do
  exists <- doesFileExist fn
  if not exists
    then pure S.empty
    else do
      ls <- BS8.lines <$> readFileBS fn
      let vs = [v | l <- ls, Right v <- [eitherDecodeStrict l]]
          moves = foldMoves [(T.toLower a, T.toLower b) | v <- vs, Right (Just (Moved a b)) <- [parseEither logLine v]]
      pure . S.fromList $
        [ followMove moves (T.toLower j)
        | v <- vs
        , Right (op, ver) <- [parseEither opVersion v]
        , op `elem` ["link", "unlink", "skip" :: Text]
        , Just (J j) <- [ver >>= parseKey]
        ]
 where
  opVersion :: Value -> Parser (Text, Maybe Text)
  opVersion = withObject "collection event" $ \o -> (,) <$> o .: "op" <*> o .:? "version"

-- append a stream link of a release (True), or its unlink (False)
appendStream :: FilePath -> ZonedTime -> AlbumKey -> Album -> Bool -> IO ()
appendStream fn now s a linked =
  appendLine fn . object $
    [ "ts" .= stamp now
    , "op" .= (if linked then "link" else "unlink" :: Text)
    , "key" .= albumID a
    , "version" .= s
    ]
      <> ["via" .= ("clutter" :: Text) | linked]
      <> ["artist" .= albumArtist a, "title" .= albumTitle a]

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
--
-- And the streams from the log: a release gets the Tidal, Apple Music and Qobuz album
-- linked there, with the provider tag. The log is the only source since 2026-11-25 (the
-- ids in the Discogs Location field were brought over once and are not read any more).
attachLinks :: Links -> Streams -> Map AlbumKey Album -> Map AlbumKey Album
attachLinks links streams am = M.mapWithKey (\k -> withStreams k . attach k) am
 where
  withStreams k a = foldr putStream a (M.findWithDefault [] k streams)
  -- the id field, the provider tag, and the Wikidata store key the id gives (Provider.wikiKeys)
  putStream st a = case st of
    T n -> (tagged "provider.tidal" . wikiKeyed ("t:" <> show n)) a{albumTidal = Just (show n)}
    A n -> (tagged "provider.applemusic" . wikiKeyed ("a:" <> show n)) a{albumAMusic = Just (show n)}
    Q q -> tagged "provider.qobuz" a{albumQobuz = Just q}
    _ -> a
  tagged t a = if t `elem` albumTags a then a else a{albumTags = albumTags a <> [t]}
  wikiKeyed w a = if w `elem` albumWikiKeys a then a else a{albumWikiKeys = albumWikiKeys a <> [w]}
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
