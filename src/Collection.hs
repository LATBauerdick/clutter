{-# LANGUAGE LambdaCase #-}

-- What is in the collection, and which versions make an album (docs/jellyfin-plan.md ›
-- Membership; docs/album-layer-plan.md).
--
-- clutter's own log, ~/Sync/clutter/collection.jsonl (CLUTTER_COLLECTION overrides): one
-- line per change, appended, never rewritten. Since the album layer (slice 2) the server
-- reads it through AlbumLayer: which versions make an album (version, split, merge, alias),
-- the keep mark of an album, moves of file albums. The lines, in short:
--
--   {"op":"format","format":2}                      the log is in the album format
--   {"op":"version","album":"C…","key":"D123"}      key is a version of that album
--   {"op":"split","key":"D123"}                     key is its own album again
--   {"op":"merge","from":"C…","into":"C…"}          album from is now into
--   {"op":"alias","from":"D9340172","to":"T1847"}   a stand-in release, read as the stream
--   {"op":"keep","key":…,"value":"yes|no|unset"}    keep mark of the album (C id or a key)
--   {"op":"moved","from":"J…","to":"J…"}            a file album's folder moved
--
-- The old link and unlink lines are not read once the log has its format line (written
-- by scripts/album-layer-migrate.py --write); a log without one has no albums of two
-- versions for this server, which says so at start.
--
-- A file album that the review of the inventory's unsure matches passed over (none of the
-- albums offered is it) gets a "skip" line, so that it is not offered again:
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
-- Every file album id in the log, and in the play log, is read through these lines.
--
-- The rule, on the album: in the collection if it is marked yes; or it is not marked and
-- has a record (a Discogs release outside the stand-in folders) or has been played; out if
-- marked no; inbox otherwise. Tags collection.in / collection.inbox / collection.out and
-- keep.yes / keep.no, on every version of the album.
module Collection (
  Moves,
  followMove,
  movedKey,
  sameKind,
  parseStreamRef,
  collectionFile,
  readAlbumLog,
  buildLayer,
  attachAlbums,
  attachAlbumPlays,
  spreadAlbumTags,
  attachCurated,
  ratedTags,
  readCurated,
  appendTag,
  appendRate,
  attachCollection,
  shelfIndexTags,
  standInFolderTags,
  appendMark,
  appendVersion,
  appendSplit,
  appendLead,
  appendMerge,
  newAlbumId,
  appendSkip,
  appendNotSame,
  notSame,
  reviewedFiles,
  readMoves,
) where

import qualified AlbumLayer as AL
import Data.Aeson (Value, eitherDecodeStrict, encode, object, withObject, (.!=), (.:), (.:?), (.=))
import Data.Aeson.Types (Pair, Parser, parseEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Hashable (hash)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (ZonedTime, defaultTimeLocale, formatTime)
import Listened (playedTags)
import Relude
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.IO (SeekMode (..), hFileSize, hPutStrLn, hSeek)
import Types (Album (..), AlbumKey (..), Layer (..), PlayEvent (..), nameOf, parseKey, rowOf)

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

-- a file album's old Jellyfin id -> the id its folder has since it moved
type Moves = Map Text Text

-- the id a file album has now: the moves followed to the end (a loop stops where it closes)
followMove :: Moves -> Text -> Text
followMove = AL.followChain

movedKey :: Moves -> AlbumKey -> AlbumKey
movedKey mv = \case
  J j -> J (followMove mv j)
  k -> k

-- both Tidal albums, both Apple albums, or both Qobuz albums
sameKind :: AlbumKey -> AlbumKey -> Bool
sameKind (T _) (T _) = True
sameKind (A _) (A _) = True
sameKind (Q _) (Q _) = True
sameKind _ _ = False

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

-- a move line, for the readers here that only need the moves
movedLine :: Value -> Parser (Maybe (Text, Text))
movedLine = withObject "collection event" $ \o -> do
  op <- o .: "op"
  if op /= ("moved" :: Text)
    then pure Nothing
    else do
      f <- o .: "from"
      t <- o .: "to"
      case (parseKey f, parseKey t) of
        (Just (J a), Just (J b)) -> pure (Just (a, b))
        _ -> fail "not a move of a file album"

movesOf :: [Value] -> Moves
movesOf vs = AL.foldChain [m | v <- vs, Right (Just m) <- [parseEither movedLine v]]

-- the album layer as the log leaves it (AlbumLayer): versions, merges, aliases, keep
-- marks, tags, ratings, lead lines, moves. A line this reader knows but cannot read is
-- skipped with a warning; lines it does not know (skip, notsame, …) are left to the others.
readAlbumLog :: FilePath -> IO AL.AlbumLog
readAlbumLog fn = do
  exists <- doesFileExist fn
  ls <- if exists then BS8.lines <$> readFileBS fn else pure []
  es <- fmap catMaybes . forM (zip [1 :: Int ..] ls) $ \(n, l) ->
    if BS8.all (`elem` [' ', '\t', '\r']) l
      then pure Nothing
      else case eitherDecodeStrict l >>= parseEither AL.logEvent of
        Right e -> pure e
        Left err -> do
          hPutStrLn stderr $ "WARNING: album layer skips " <> fn <> ":" <> show n <> ": " <> err
          pure Nothing
  pure (AL.foldLog es)

--
-- The album layer on the albums clutter holds (docs/album-layer-plan.md › Spec)
--

-- Which album each key clutter holds belongs to, and the key of that album's row: its
-- lead version among the versions held (AlbumLayer.leadAmong; records by the date they
-- were added). Besides:
--  - a key the log reads as another (a stand-in release with an alias, an old file album
--    id) is no version: its row is that other key's album's;
--  - an Apple Music album that a Tidal album names through the store lookup, and that is
--    no version of an album, has the Tidal album's row (an "also on Apple Music" fact of
--    that stream, docs/album-layer-plan.md › Migration, 6);
--  - a play counts for the album of the first of its keys clutter holds (the one its src
--    names, then discogs, tidal, apple, jellyfin), read through moves and aliases.
buildLayer :: AL.AlbumLog -> Map AlbumKey Album -> Layer
buildLayer al am =
  Layer
    { lyFormat = AL.alFormat al
    , lyName = names
    , lyRow = rows
    , lyVersions = versions
    , lyKeep = AL.alKeep al
    , lyRated = AL.alRated al
    , lyLeadSet = S.fromList [n | (n, k) <- M.toList (AL.alLead al), k `elem` M.findWithDefault [] n heldOrdered]
    , lyStandIns = M.fromListWith (flip (<>)) [(n, [k]) | (k, n) <- M.toList aliased]
    , lyIds = S.fromList (M.keys (AL.albumsWithIds al)) <> S.fromList (map C (M.keys (AL.alMerges al) <> M.elems (AL.alMerges al)))
    , -- a play counts if clutter holds its version, or its version is one of an album
      -- clutter shows (a Tidal album that is no favourite, of a record)
      lyPlay = \e -> twinName . AL.albumOfKey al <$> AL.playVersion counts al (playKeys e)
    }
 where
  held k = M.member k am
  counts k = held k || M.member (AL.albumOfKey al k) heldOf
  canon = AL.canonical al
  own = [k | k <- M.keys am, canon k == k]
  added k = case k of D _ -> mfilter (not . T.null) (albumAdded <$> M.lookup k am); _ -> Nothing
  -- every album with an id -> its versions held, lead first, in the order they joined.
  -- An album none of whose versions clutter holds (a Tidal and an Apple album that are no
  -- favourites) is shown by the stand-in release read as one of them, as before the
  -- migration: that stand-in is its row, though no version.
  heldOwn = M.fromListWith (flip (<>)) [(AL.albumOfKey al k, [k]) | k <- own]
  -- (a second stand-in of the same album has that first one's row: aliasRows)
  heldOf = M.unionWith (\o _ -> o) heldOwn (M.map (take 1) (M.fromListWith (flip (<>)) [(n, [k]) | (k, n) <- M.toList aliased, not (M.member n heldOwn)]))
  ordered n ks = case AL.leadAmong (`elem` ks) added al n of
    Just l -> l : filter (/= l) (inLog n ks)
    Nothing -> inLog n ks
  inLog n ks = case n of
    C _ -> let vs = AL.versionsOf al n in filter (`elem` ks) vs <> filter (`notElem` vs) ks
    _ -> ks
  heldOrdered = M.mapWithKey ordered heldOf
  -- an album with an id: the versions held (lead first), then the others in the log
  -- (and an album of one version clutter does not hold, shown by a stand-in: the stand-in,
  -- then that version)
  versions =
    M.mapWithKey (\n vs -> let hs = M.findWithDefault [] n heldOrdered in hs <> filter (`notElem` hs) vs) $
      M.fromList [(n, AL.versionsOf al n) | n <- M.keys (AL.albumsWithIds al)]
        <> M.fromList [(n, [n]) | n <- M.keys heldOf, not (isId n), not (M.member n heldOwn)]
  isId = \case C _ -> True; _ -> False
  leadOfName n = M.lookup n heldOrdered >>= viaNonEmpty head
  ownNames = M.fromList [(k, AL.albumOfKey al k) | k <- own]
  -- a key read as another: the album of the key it is read as
  aliased = M.fromList [(k, AL.albumOfKey al k) | k <- M.keys am, canon k /= k]
  ownRows = M.fromList [(k, l) | (k, n) <- M.toList ownNames, Just l <- [leadOfName n], l /= k]
  aliasRows = M.fromList [(k, l) | (k, n) <- M.toList aliased, Just l <- [leadOfName n], l /= k]
  -- the Apple Music album of a Tidal album (store lookup), where it stands alone: shown as
  -- the Tidal album's row and counted as its album (plays, keep mark, collection), as the
  -- old server hid it behind the Tidal album
  twins =
    [ (A i, k)
    | (k@(T _), a) <- M.toList am
    , Just i <- [readMaybe . toString =<< albumAMusic a]
    , held (A i)
    , M.lookup (A i) ownNames == Just (A i)
    ]
  twinOf = M.fromList [(t, M.findWithDefault k k ownNames) | (t, k) <- twins]
  twinName n = M.findWithDefault n n twinOf
  appleRows = M.fromList [(t, M.findWithDefault k k ownRows) | (t, k) <- twins]
  names = M.map twinName ownNames <> aliased
  rows = ownRows <> aliasRows <> appleRows

-- the keys of a play: the one its src names, then the others in the order discogs, tidal,
-- apple, jellyfin
playKeys :: PlayEvent -> [AlbumKey]
playKeys e = ordNub (catMaybes [src, D <$> peDiscogs e, T <$> peTidal e, A <$> peApple e, J . T.toLower <$> peJellyfin e])
 where
  src = case peSrc e of
    "discogs" -> D <$> peDiscogs e
    "tidal" -> T <$> peTidal e
    "apple" -> A <$> peApple e
    "jellyfin" -> J . T.toLower <$> peJellyfin e
    _ -> Nothing

-- tags of an album that are about the album, not about one version: on every version
albumLevel :: Text -> Bool
albumLevel t = any (`T.isPrefixOf` t) ["played.", "collection.", "keep.", "length.", "rated.", "my."]

-- tags a version shows of what else its album is available as (provider.*, storage.*,
-- and Tidal's own: in the collection, gone)
borrowed :: Text -> Bool
borrowed t = any (`T.isPrefixOf` t) ["provider.", "storage.", "tidal."]

-- Put each album's versions on the albums clutter holds. The lead version is the album's
-- row: it gets the album's streams (albumTidal, albumAMusic, albumQobuz: the first of each
-- in the log, held or not), its file albums (albumFiles, albumJellyfin), the union of its
-- versions' own tags (an album with a vinyl and a CD is found under both formats), plus
-- provider.* for the streams clutter does not hold, and the Wikidata store keys of all
-- versions, the lead's first. Every other version keeps its own identity (a Tidal album's
-- albumTidal is its own) and its own tags, with the album's provider.*, storage.* and
-- tidal.* tags; it gets the album's streams and files too, so that its page shows them.
-- Works on the albums as the readers give them (Env.baseR).
attachAlbums :: Layer -> Map AlbumKey Album -> Map AlbumKey Album
attachAlbums ly am = foldl' putAlbum am (M.elems (lyVersions ly))
 where
  putAlbum m vs =
    let hs = filter (`M.member` m) vs
     in case hs of
          [] -> m
          (l : rest) ->
            let la = m M.! l
                others = [m M.! k | k <- rest]
                firstT = viaNonEmpty head [show n | T n <- vs]
                firstA = viaNonEmpty head [show n | A n <- vs]
                firstQ = viaNonEmpty head [q | Q q <- vs]
                files = [j | J j <- hs]
                unheld = filter (`notElem` hs) vs
                streamTags = ["provider.tidal" | T _ <- unheld] <> ["provider.applemusic" | A _ <- unheld] <> ["provider.qobuz" | Q _ <- unheld]
                streamWiki = ["t:" <> show n | T n <- unheld] <> ["a:" <> show n | A n <- unheld]
                -- album-level tags (rated.*, …) come from one version (spreadAlbumTags), not the union
                rowTags = ordNub (albumTags la <> filter (not . albumLevel) (concatMap albumTags others) <> streamTags)
                withStreams a =
                  a
                    { albumTidal = case albumID a of T _ -> albumTidal a; _ -> firstT <|> albumTidal a
                    , albumAMusic = case albumID a of A _ -> albumAMusic a; T _ -> albumAMusic a <|> firstA; _ -> firstA <|> albumAMusic a
                    , albumQobuz = firstQ <|> albumQobuz a
                    , albumFiles = case albumID a of J j -> filter (/= j) files; _ -> files
                    , albumJellyfin = case albumID a of J _ -> albumJellyfin a; _ -> viaNonEmpty head files <|> albumJellyfin a
                    }
                lead =
                  (withStreams la)
                    { albumTags = rowTags
                    , albumWikiKeys = ordNub (albumWikiKeys la <> concatMap albumWikiKeys others <> streamWiki)
                    }
                other a = (withStreams a){albumTags = ordNub (albumTags a <> filter borrowed rowTags)}
             in foldl' (\m' a -> M.insert (albumID a) (other a) m') (M.insert l lead m) others

-- albumListenedDates and the played.* tags, by album: a play counts for the album its
-- version belongs to (Layer.lyPlay), and every version of the album shows the album's
-- plays. played.early: the Discogs field 7 mark of any of its records.
attachAlbumPlays :: Layer -> [PlayEvent] -> Map AlbumKey Album -> Map AlbumKey Album
attachAlbumPlays ly es am = M.mapWithKey attach am
 where
  dates = M.fromListWith (<>) [(n, [peTs e]) | e <- sortOn peTs es, Just n <- [lyPlay ly e]]
  early = M.fromListWith max [(nameOf ly k, albumPlays a) | (k, a) <- M.toList am]
  attach k a =
    let n = nameOf ly k
        ds = M.findWithDefault [] n dates
     in a
          { albumListenedDates = ds
          , albumTags = filter (not . ("played." `T.isPrefixOf`)) (albumTags a) <> playedTags (length ds) (M.findWithDefault 0 n early)
          }

-- The curated tags (my.*) and clutter's own rating, from the collection log, on every
-- version of an album (and on its stand-ins). The rating replaces the rated.* tags from
-- Discogs once the log has a rate line (seeded by scripts/import-discogs-curation.py),
-- and albumRating with it (the list cards' stars); before that the Discogs ones stay
-- (spreadAlbumTags). Run after spreadAlbumTags.
attachCurated :: AL.AlbumLog -> Layer -> Map AlbumKey Album -> Map AlbumKey Album
attachCurated alog ly = M.mapWithKey putCurated
 where
  putCurated k a =
    let n = nameOf ly k
        mine = S.toList (AL.tagsOf alog n)
        stars = fromMaybe 0 (AL.ratingOf alog n)
        rated = if AL.alRated alog then ratedTags stars else []
        keep t = not ("my." `T.isPrefixOf` t) && not (AL.alRated alog && "rated." `T.isPrefixOf` t)
     in a
          { albumTags = filter keep (albumTags a) <> mine <> rated
          , albumRating = if AL.alRated alog then stars else albumRating a
          }

-- the rated.* tags of a rating (0: none), as FromDiscogs gives them for a Discogs rating
ratedTags :: Int -> [Text]
ratedTags = \case
  0 -> ["rated.not"]
  1 -> ["rated.", "rated.*", "rated.dislike"]
  2 -> ["rated.", "rated.**", "rated.dislike"]
  3 -> ["rated.", "rated.***"]
  4 -> ["rated.", "rated.****", "rated.like"]
  _ -> ["rated.", "rated.*****", "rated.like"]

-- The curated tags clutter offers (~/Sync/clutter/curated-tags.json, $CLUTTER_CURATED_TAGS:
-- [{"tag":"my.piano","name":"Piano"}, …], in the sidebar's order), then any my.* tag in use
-- that the file does not name (its name: the tag without "my."). A missing or broken file
-- is an empty list.
readCurated :: [Text] -> IO [(Text, Text)]
readCurated inUse = do
  fn <-
    lookupEnv "CLUTTER_CURATED_TAGS" >>= \case
      Just f | not (null f) -> pure f
      _ -> (\h -> h </> "Sync" </> "clutter" </> "curated-tags.json") . fromMaybe "." <$> lookupEnv "HOME"
  ok <- doesFileExist fn
  listed <-
    if not ok
      then pure []
      else do
        bs <- readFileBS fn
        pure . either (const []) id $
          eitherDecodeStrict bs >>= parseEither (mapM (withObject "curated" (\o -> (,) <$> (T.toLower <$> o .: "tag") <*> o .: "name")))
  let known = S.fromList (map fst listed)
      extra = [(t, T.drop 3 t) | t <- ordNub (sort inUse), "my." `T.isPrefixOf` t, not (S.member t known)]
  pure (listed <> extra)

-- The album-level tags that come from one version: length.* (the lead's) and rated.* (from
-- Discogs until clutter has its own rating: the first record of the album that is rated,
-- lead first), the same on every version of an album. Run after Details.attachLength.
spreadAlbumTags :: Layer -> Map AlbumKey Album -> Map AlbumKey Album
spreadAlbumTags ly am = foldl' spread am albums
 where
  pre p t = p `T.isPrefixOf` t
  -- every album with an id or a stand-in: its held versions (lead first) and its stand-ins
  albums =
    [ (filter (`M.member` am) vs, M.findWithDefault [] n (lyStandIns ly))
    | n <- ordNub (M.keys (lyVersions ly) <> M.keys (lyStandIns ly))
    , let vs = M.findWithDefault [n] n (lyVersions ly)
    ]
  spread m (hs, sis) = case hs of
    l : _ | length hs + length sis >= 2 ->
      let tagsOf k = maybe [] albumTags (M.lookup k m)
          lengthTags = filter (pre "length.") (tagsOf l)
          -- the first version (lead first), then stand-in, that is rated
          rated = filter (pre "rated.") <$> map tagsOf (hs <> sis)
          ratedFirst = fromMaybe (fromMaybe [] (viaNonEmpty head (filter (not . null) rated))) (find (\ts -> not (null ts) && "rated.not" `notElem` ts) rated)
          setTags a = a{albumTags = filter (\t -> not (pre "length." t || pre "rated." t)) (albumTags a) <> lengthTags <> ratedFirst}
       in foldl' (flip (M.adjust setTags)) m (hs <> sis)
    _ -> m

-- set the collection.* and keep.* tags, by album (Layer.lyKeep, the rule above). Needs the
-- played.* tags (attachAlbumPlays) and the albums as the readers give them (for a record's
-- folder: a stand-in is no record).
attachCollection :: Set Text -> Layer -> Map AlbumKey Album -> Map AlbumKey Album -> Map AlbumKey Album
attachCollection standInTags ly base am = M.mapWithKey setTags am
 where
  isRecord (k, a) = case k of
    D _ -> not (any (`S.member` standInTags) (albumTags a))
    _ -> False
  withRecord = S.fromList [nameOf ly k | (k, a) <- M.toList base, isRecord (k, a)]
  setTags k a =
    let n = nameOf ly k
        tags = filter (\t -> not ("collection." `T.isPrefixOf` t || "keep." `T.isPrefixOf` t)) (albumTags a)
        played = "played.never" `notElem` tags
        new = case M.lookup n (lyKeep ly) of
          Just True -> ["keep.yes", "collection.in"]
          Just False -> ["keep.no", "collection.out"]
          Nothing -> [if S.member n withRecord || played then "collection.in" else "collection.inbox"]
     in a{albumTags = tags <> new}

-- The tags a record matches in a shelf list, where it differ from its album's row: a
-- record that leads an album of several versions matches on its own tags (format,
-- folder, …), the album-level ones, and what else the album is available as (provider.*,
-- storage.*, tidal.*) — not on another record's format. Every other key matches in a
-- shelf list as it does in the others.
shelfIndexTags :: Layer -> Map AlbumKey Album -> Map AlbumKey Album -> Map AlbumKey [Text]
shelfIndexTags ly base am =
  M.fromList
    -- the record's own tags without its album-level ones (its Discogs rated.*, its own
    -- played.*): those are the album's, from a
    [ (l, ordNub (filter (not . albumLevel) (albumTags b) <> filter (\t -> albumLevel t || borrowed t) (albumTags a)))
    | vs <- M.elems (lyVersions ly)
    , l@(D _) : _ : _ <- [filter (`M.member` am) vs]
    , rowOf ly l == l
    , Just b <- [M.lookup l base]
    , Just a <- [M.lookup l am]
    ]

--
-- Writing the log
--

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

-- for a human reader of the log; never read back
named :: Album -> [Pair]
named a = ["artist" .= albumArtist a, "title" .= albumTitle a]

-- append one keep mark, against the album's name (its C id, or its one version's key)
appendMark :: FilePath -> ZonedTime -> AlbumKey -> Album -> Maybe Bool -> IO ()
appendMark fn now n a mark =
  appendLine fn . object $
    [ "ts" .= stamp now
    , "op" .= ("keep" :: Text)
    , "key" .= n
    , "value" .= (case mark of Just True -> "yes"; Just False -> "no"; Nothing -> "unset" :: Text)
    ]
      <> named a

-- a version joins an album with an id
appendVersion :: FilePath -> ZonedTime -> AlbumKey -> AlbumKey -> Album -> IO ()
appendVersion fn now c k a =
  appendLine fn . object $
    ["ts" .= stamp now, "op" .= ("version" :: Text), "album" .= c, "key" .= k, "via" .= ("clutter" :: Text)] <> named a

-- the version that leads an album (named by its C id); Nothing: back to the rule
appendLead :: FilePath -> ZonedTime -> AlbumKey -> Maybe AlbumKey -> Album -> IO ()
appendLead fn now n mk a =
  appendLine fn . object $
    ["ts" .= stamp now, "op" .= ("lead" :: Text), "album" .= n] <> maybe [] (\k -> ["key" .= k]) mk <> ["via" .= ("clutter" :: Text)] <> named a

-- a curated tag on the album, or taken off (value false)
appendTag :: FilePath -> ZonedTime -> AlbumKey -> Text -> Bool -> Album -> IO ()
appendTag fn now n t v a =
  appendLine fn . object $
    ["ts" .= stamp now, "op" .= ("tag" :: Text), "album" .= n, "tag" .= t, "value" .= v, "via" .= ("clutter" :: Text)] <> named a

-- the album's rating, 1–5; 0 takes it off
appendRate :: FilePath -> ZonedTime -> AlbumKey -> Int -> Album -> IO ()
appendRate fn now n s a =
  appendLine fn . object $
    ["ts" .= stamp now, "op" .= ("rate" :: Text), "album" .= n, "stars" .= s, "via" .= ("clutter" :: Text)] <> named a

-- a version is its own album again
appendSplit ::FilePath -> ZonedTime -> AlbumKey -> Album -> IO ()
appendSplit fn now k a =
  appendLine fn . object $
    ["ts" .= stamp now, "op" .= ("split" :: Text), "key" .= k, "via" .= ("clutter" :: Text)] <> named a

-- album from is now album into (both with ids)
appendMerge :: FilePath -> ZonedTime -> AlbumKey -> AlbumKey -> Album -> IO ()
appendMerge fn now from into a =
  appendLine fn . object $
    ["ts" .= stamp now, "op" .= ("merge" :: Text), "from" .= from, "into" .= into, "via" .= ("clutter" :: Text)] <> named a

-- a new album id: C + 6 characters of 0-9 and a-z without i l o u, from a hash of the
-- versions and the time, not one of those taken
newAlbumId :: ZonedTime -> [AlbumKey] -> Set AlbumKey -> AlbumKey
newAlbumId now ks taken = fromMaybe (C "000000") (find (`S.notMember` taken) (map cid [0 :: Int ..]))
 where
  alphabet = "0123456789abcdefghjkmnpqrstvwxyz" :: Text
  seed = T.intercalate "|" (map show (sort ks)) <> "@" <> stamp now
  cid n =
    let h = abs (hash (seed <> "#" <> show n))
     in C (toText [T.index alphabet ((h `div` (32 ^ i)) `mod` 32) | i <- [0 .. 5 :: Int]])

-- append a skip: the review of the unsure matches found none of the albums offered to be
-- this file album (the album given is the file album itself)
appendSkip :: FilePath -> ZonedTime -> Album -> IO ()
appendSkip fn now a = case albumID a of
  J j -> appendLine fn . object $ ["ts" .= stamp now, "op" .= ("skip" :: Text), "version" .= J j] <> named a
  _ -> pure ()

-- the moves alone, for a reader that has the old ids (the inventory's TSV)
readMoves :: FilePath -> IO Moves
readMoves fn = do
  exists <- doesFileExist fn
  if not exists
    then pure M.empty
    else do
      ls <- BS8.lines <$> readFileBS fn
      pure $ movesOf [v | l <- ls, "\"moved\"" `BS.isInfixOf` l, Right v <- [eitherDecodeStrict l]]

-- append that a suggestion is not the same album as this one (True), or take that back
appendNotSame :: FilePath -> ZonedTime -> AlbumKey -> Album -> Bool -> IO ()
appendNotSame fn now this cand excluded =
  appendLine fn . object $
    [ "ts" .= stamp now
    , "op" .= ("notsame" :: Text)
    , "version" .= this
    , "key" .= albumID cand
    ]
      <> (if excluded then named cand else ["undo" .= True])

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
          k = movedKey (movesOf vs)
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

-- the file albums (Jellyfin ids, as they are now) that the log has decided: a link, unlink
-- or skip line ("version"), or a version or split line ("key") for it — by hand or by the
-- import — not offered for review again
reviewedFiles :: FilePath -> IO (Set Text)
reviewedFiles fn = do
  exists <- doesFileExist fn
  if not exists
    then pure S.empty
    else do
      ls <- BS8.lines <$> readFileBS fn
      let vs = [v | l <- ls, Right v <- [eitherDecodeStrict l]]
          moves = movesOf vs
      pure . S.fromList $
        [ followMove moves (T.toLower j)
        | v <- vs
        , Right (op, ver, key) <- [parseEither opVersion v]
        , Just (J j) <- [parseKey =<< if op `elem` ["link", "unlink", "skip" :: Text] then ver else if op `elem` ["version", "split"] then key else Nothing]
        ]
 where
  opVersion :: Value -> Parser (Text, Maybe Text, Maybe Text)
  opVersion = withObject "collection event" $ \o -> (,,) <$> o .: "op" <*> o .:? "version" <*> o .:? "key"
