{-# LANGUAGE LambdaCase #-}

-- The album layer, read from the collection log (docs/album-layer-plan.md › Spec): which
-- versions make an album, what is recorded against an album, and which version leads.
-- Pure: the lines come in as JSON values (Collection.readAlbumLog reads the file), and
-- scripts/collection_log.py gives the same answers for the scripts.
--
-- An album with one version has no id of its own: it is named by that version's key. An
-- album with two or more has a C id ("C3fk9qz"). Lines that speak about an album name it
-- by its C id or by any version key; every name is resolved through the membership as
-- the whole log leaves it, not as it was when the line was written.
--
--   {"op":"format","format":2}                      link/unlink lines are no longer read
--   {"op":"version","album":"C…","key":"D123"}      key is a version of that album
--   {"op":"split","key":"D123"}                     key is its own album again
--   {"op":"merge","from":"C…","into":"C…"}          album from is now into
--   {"op":"lead","album":…,"key":"D123"}            that version leads; no key: the rule
--   {"op":"alias","from":"D9340172","to":"T1847"}   from is read as to (a stand-in gone)
--   {"op":"keep","key":…,"value":"yes|no|unset"}    keep mark of the album
--   {"op":"tag","album":…,"tag":"my.piano","value":true}
--   {"op":"rate","album":…,"stars":4}               0 takes the rating off
--   {"op":"moved","from":"J…","to":"J…"}            a file album's new Jellyfin id
--
-- Last line wins for version/split (per key), keep, rate and lead (per album, after
-- resolving); tags are folded in order. Moves, aliases and merges are chains, followed to
-- the end; a loop stops where it closes. For moves a line to an id takes off a line from
-- it (foldChain); for merges and aliases only the exact reverse line (b → a after a → b)
-- takes the earlier one off, so merge b → c then a → b ends with a in c (foldLinks).
-- Where a version key is meant (version, split, the key of lead, both ends of alias) a C
-- id is not read.
module AlbumLayer (
  AlbumLog (..),
  Event (..),
  logEvent,
  foldLog,
  canonical,
  albumOfKey,
  resolveName,
  versionsOf,
  albumsWithIds,
  keepOf,
  tagsOf,
  ratingOf,
  leadOf,
  leadAmong,
  kindRank,
  playVersion,
  foldChain,
  followChain,
) where

import Data.Aeson (Value, withObject, (.:), (.:?))
import Data.Aeson.Types (Parser)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Relude
import Types (AlbumKey (..), parseKey)

data Event
  = Format
  | Version Text AlbumKey -- C id (without the C), key
  | Split AlbumKey
  | Merge Text Text -- from, into (C ids without the C)
  | Lead AlbumKey (Maybe AlbumKey) -- album name, version
  | Alias AlbumKey AlbumKey -- from, to
  | KeepMark AlbumKey (Maybe Bool)
  | Tag AlbumKey Text Bool
  | Rate AlbumKey Int
  | Moved Text Text -- Jellyfin ids, lower case
  deriving (Eq, Show)

-- Nothing for a line this reader leaves to others (link, unlink, skip, notsame, …)
logEvent :: Value -> Parser (Maybe Event)
logEvent = withObject "collection event" $ \o -> do
  op <- o .: "op"
  let key f = o .: f >>= \t -> maybe (fail ("not an album key: " <> toString t)) pure (parseKey t)
      cid f = key f >>= \case C c -> pure c; _ -> fail "not an album id (C…)"
      -- a version key: an album id where a version is meant would make an album a version
      vkey f = key f >>= \case C _ -> fail "an album id where a version key is meant"; k -> pure k
  case op :: Text of
    "format" -> pure (Just Format)
    "version" -> Just <$> (Version <$> cid "album" <*> vkey "key")
    "split" -> Just . Split <$> vkey "key"
    "merge" -> Just <$> (Merge <$> cid "from" <*> cid "into")
    "lead" -> do
      a <- key "album"
      mk <- o .:? "key"
      v <- traverse (\t -> case parseKey t of Just (C _) -> fail "an album id where a version key is meant"; Just k -> pure k; Nothing -> fail "not a version key") mk
      pure (Just (Lead a v))
    "alias" -> Just <$> (Alias <$> vkey "from" <*> vkey "to")
    "keep" -> do
      k <- key "key"
      v <- o .: "value"
      case v :: Text of
        "yes" -> pure (Just (KeepMark k (Just True)))
        "no" -> pure (Just (KeepMark k (Just False)))
        "unset" -> pure (Just (KeepMark k Nothing))
        _ -> fail "not a keep mark"
    "tag" -> do
      a <- key "album"
      t <- o .: "tag"
      v <- o .: "value"
      pure (Just (Tag a (T.toLower t) v))
    "rate" -> do
      a <- key "album"
      s <- o .: "stars"
      if s < 0 || s > 5 then fail "stars out of 0–5" else pure (Just (Rate a s))
    "moved" ->
      key "from" >>= \f ->
        key "to" >>= \t -> case (f, t) of
          (J a, J b) -> pure (Just (Moved a b))
          _ -> fail "not a move of a file album"
    _ -> pure Nothing

-- the moves as the lines leave them: a line to an id takes off a line from it (a folder
-- moved back, or moved on), so that following ends
foldChain :: Ord a => [(a, a)] -> Map a a
foldChain = foldl' (\m (a, b) -> M.insert a b (M.delete b m)) M.empty

-- merges and aliases as the lines leave them: a later line from an id replaces the earlier
-- one; only the exact reverse (b → a after a → b) takes the earlier one off. A chain
-- a → b → c stays (merge b into c, then a into b: all of a's versions end in c);
-- followChain ends any cycle.
foldLinks :: Ord a => [(a, a)] -> Map a a
foldLinks = foldl' (\m (a, b) -> M.insert a b (if M.lookup b m == Just a then M.delete b m else m)) M.empty

-- followed to the end; a loop stops where it closes
followChain :: Ord a => Map a a -> a -> a
followChain m = go S.empty
 where
  go seen x = case M.lookup x m of
    Just y | y /= x, not (S.member y seen) -> go (S.insert x seen) y
    _ -> x

data AlbumLog = AlbumLog
  { alFormat :: Bool -- a format line: the log is in the album format
  , alMoves :: Map Text Text
  , alAliases :: Map AlbumKey AlbumKey -- from (moves applied) -> to
  , alMerges :: Map Text Text -- C id -> the one it was merged into
  , alMember :: Map AlbumKey (Text, Int) -- canonical version key -> its album's C id (as written) and the line that made it a member
  , alAlbums :: Map AlbumKey [(AlbumKey, Int)] -- every album with an id -> its versions in the order they joined (computed once)
  , alKeep :: Map AlbumKey Bool -- by album
  , alTags :: Map AlbumKey (Set Text) -- by album
  , alRate :: Map AlbumKey Int -- by album
  , alLead :: Map AlbumKey AlbumKey -- by album: the version a lead line names
  }

-- the key a version is known by now: moves and aliases followed (a loop stops)
canonical :: AlbumLog -> AlbumKey -> AlbumKey
canonical al = go S.empty
 where
  go seen k =
    let k1 = case k of J j -> J (followChain (alMoves al) j); _ -> k
     in if S.member k1 seen
          then k1
          else case M.lookup k1 (alAliases al) of
            Just k2 | k2 /= k1 -> go (S.insert k1 seen) k2
            _ -> k1

-- the album a version key belongs to: its C id, or the key itself if it stands alone
albumOfKey :: AlbumLog -> AlbumKey -> AlbumKey
albumOfKey al k =
  let k' = canonical al k
   in maybe k' (\(c, _) -> C (followChain (alMerges al) c)) (M.lookup k' (alMember al))

-- the album a line names, by C id or by version key
resolveName :: AlbumLog -> AlbumKey -> AlbumKey
resolveName al = \case
  C c -> C (followChain (alMerges al) c)
  k -> albumOfKey al k

-- every album with an id -> its versions with the line that made each a member, in that order
albumsWithIds :: AlbumLog -> Map AlbumKey [(AlbumKey, Int)]
albumsWithIds = alAlbums

-- an album's versions in the order they joined; an album without an id is its one key
versionsOf :: AlbumLog -> AlbumKey -> [AlbumKey]
versionsOf al name = case resolveName al name of
  a@(C _) -> map fst (M.findWithDefault [] a (alAlbums al))
  k -> [k]

keepOf :: AlbumLog -> AlbumKey -> Maybe Bool
keepOf al n = M.lookup (resolveName al n) (alKeep al)

tagsOf :: AlbumLog -> AlbumKey -> Set Text
tagsOf al n = M.findWithDefault S.empty (resolveName al n) (alTags al)

ratingOf :: AlbumLog -> AlbumKey -> Maybe Int
ratingOf al n = M.lookup (resolveName al n) (alRate al)

-- the lead rule: a record, then Tidal, files, Apple Music, Qobuz
kindRank :: AlbumKey -> Int
kindRank = \case D _ -> 0; T _ -> 1; J _ -> 2; A _ -> 3; Q _ -> 4; C _ -> 5

-- The version that leads an album: the one a lead line names if it is still a version;
-- else by kindRank, records by the date they were added to the collection (the earliest;
-- no date last), the others by when they became a version.
leadOf :: Ord d => (AlbumKey -> Maybe d) -> AlbumLog -> AlbumKey -> Maybe AlbumKey
leadOf = leadAmong (const True)

-- the same among the versions clutter holds: a Tidal album that is no favourite, a Qobuz
-- album, a file album gone from the server cannot lead a row (the server's choice)
leadAmong :: Ord d => (AlbumKey -> Bool) -> (AlbumKey -> Maybe d) -> AlbumLog -> AlbumKey -> Maybe AlbumKey
leadAmong holds added al name =
  let a = resolveName al name
      vs = filter (holds . fst) $ case a of
        C _ -> M.findWithDefault [] a (alAlbums al)
        k -> [(k, 0)]
      named = M.lookup a (alLead al) >>= \k -> find ((== k) . fst) vs
      order (k, n) = (kindRank k, if kindRank k == 0 then maybe (1 :: Int, Nothing) (\d -> (0, Just d)) (added k) else (0, Nothing), n)
   in fst <$> (named <|> viaNonEmpty head (sortOn order vs))

-- A play's version: the first of its keys (the one its src names, then the others in the
-- order discogs, tidal, apple, jellyfin) that clutter holds, read through moves and aliases.
playVersion :: (AlbumKey -> Bool) -> AlbumLog -> [AlbumKey] -> Maybe AlbumKey
playVersion holds al = find holds . map (canonical al)

-- the album layer as the whole log leaves it; the events in the order of the log
foldLog :: [Event] -> AlbumLog
foldLog es =
  let moves = foldChain [(a, b) | Moved a b <- es]
      mv = \case J j -> J (followChain moves j); k -> k
      aliases = foldLinks [(mv a, mv b) | Alias a b <- es]
      merges = foldLinks [(a, b) | Merge a b <- es]
      base = AlbumLog True moves aliases merges M.empty M.empty M.empty M.empty M.empty M.empty
      member =
        foldl'
          ( \m (n, e) -> case e of
              Version c k -> M.insert (canonical base k) (c, n) m
              Split k -> M.delete (canonical base k) m
              _ -> m
          )
          M.empty
          (zip [0 ..] es)
      table =
        M.map (sortOn snd) $
          M.fromListWith (<>) [(C (followChain merges c), [(k, n)]) | (k, (c, n)) <- M.toList member]
      al0 = base{alFormat = Format `elem` es, alMember = member, alAlbums = table}
      r = resolveName al0
      step al = \case
        KeepMark n v -> al{alKeep = M.alter (const v) (r n) (alKeep al)}
        Tag n t v -> al{alTags = M.filter (not . S.null) (M.alter (Just . (if v then S.insert t else S.delete t) . fromMaybe S.empty) (r n) (alTags al))}
        Rate n s -> al{alRate = M.alter (const (if s == 0 then Nothing else Just s)) (r n) (alRate al)}
        Lead n mk -> al{alLead = M.alter (const (canonical al0 <$> mk)) (r n) (alLead al)}
        _ -> al
   in foldl' step al0 es
