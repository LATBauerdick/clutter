{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Env (
  envUpdate,
  envInit,
  envUpdateAlbum,
  envGetAlbum,
  envRefreshAlbum,
  envTidalConnect,
  envTidalReload,
  envJellyfinReload,
  envGetTag,
  envUpdateSort,
  envUpdateMenuParams,
  envRefreshPlays,
  envSetMark,
  envSetMarks,
  envSetLink,
  envSetStream,
)
where

import Data.Aeson (FromJSON (..), decode)
import qualified Data.List as L (
  union,
 )
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Text as T (all, find, length, stripPrefix)
import Data.Vector (Vector)
import qualified Data.Vector as V (
  empty,
  fromList,
  null,
  reverse,
  toList,
 )
import Provider (
  -- extractListenedDates,
  readAMusicAlbums,
  readAlbum,
  readAlbums,
  readDiscogsAlbums,
  readDiscogsFolders,
  readDiscogsListAids,
  readDiscogsLists,
  readFolderAids,
  readFolders,
  readTidalAlbums,
  readTidalDropped,
  linkTidalTags,
  memberList,
  serverList,
  allList,
  updateAMusicFolderAids,
  updateTidalFolderAids,
 )

import Wikidata (attachWiki, readWikiStore)
import Details (attachLength, factsFor, readDetailStore)
import qualified Data.Text as TX
import Collection (Moves, attachCollection, attachLinks, collectionFile, followMove, readCollection, sameKind, standInFolderTags)
import Jellyfin (JellyfinStore (..), jellyfinAlbums, readJellyfinStore)
import Listened (attachPlays, listenedDir, listenedName, listenedRows, listenedYears, readPlays)
import Relude
import System.IO (hPutStrLn)
import Types (
  -- Discogs (..),

  AMusic (..),
  AMusicInfo (..),
  Album (..),
  AlbumKey (..),
  AppM,
  Discogs (..),
  Env (..),
  EnvR (..),
  MenuParams (..),
  PlayEvent (..),
  SortOrder (..),
  Tidal (..),
  TidalInfo (..),
  envGetDiscogs,
  envGetEnvr,
  pLocList,
 )

data TokJ = TokJ
  { amt :: Text
  , aky :: Text
  , tok :: Text
  , usr :: Text
  , jky :: Text
  , tid :: Text
  , tsi :: Text
  , tcc :: Text
  , tdt :: Text
  }
  deriving (Generic, Show)
instance FromJSON TokJ

-- get authorization info from a text file
getProviders :: IO (Tidal, Tidal, AMusic, Discogs, Discogs)
getProviders = do
  ejson <- (decode <$> readFileLBS "tok.json") :: IO (Maybe TokJ)
  -- ts <- case ejson of
  --   Nothing -> exitFailure
  --   -- putTextLn $ "Error: " <> show err
  --   -- putTextLn "Failed to parse JSON"
  --   Just x -> pure x
  let ts = fromJust ejson
  -- jsonData <- B.readFile "tok.json"
  -- let mts = eitherDecode jsonData :: Maybe TokJ
  -- let ts = fromJust mts
  let appleMusicDevToken = amt ts
      appleMusicUserToken = aky ts
      discogsDevToken = tok ts
      discogsUserName = usr ts
      jellyfinAPIKey = jky ts
      tidalUserId = fromMaybe 0 $ readMaybe (toString $ tid ts) :: Int
      tidalSessionId = tsi ts
      tidalCountryCode = tcc ts
      tidalAccessToken = tdt ts
  -- t <- readFileBS "tok.dat" -- for debug, get from file with authentication data
  -- let ts :: [Text]
  --     ts = words . decodeUtf8 $ t
  --     appleMusicDevToken = fromJust $ ts !!? 0 -- t6
  --     appleMusicUserToken = fromJust $ ts !!? 1 -- t7
  --     discogsDevToken = fromJust $ ts !!? 2
  --     discogsUserName = fromJust $ ts !!? 3
  --     jellyfinAPIKey = fromJust $ ts !!? 4
  --     tidalUserId = fromMaybe 0 $ readMaybe (toString . fromJust $ ts !!? 5) :: Int
  --     tidalSessionId = fromJust $ ts !!? 6 -- t3
  --     tidalCountryCode = fromJust $ ts !!? 7 -- t4
  --     tidalAccessToken = fromJust $ ts !!? 8 -- t5
  pure
    ( Tidal $ TidalSession tidalUserId tidalSessionId tidalCountryCode tidalAccessToken
    , Tidal $ TidalFile "cache/traw.json" -- Tidal from cache
    , AMusic $ AMusicSession appleMusicDevToken appleMusicUserToken
    , DiscogsSession discogsDevToken discogsUserName
    , DiscogsFile "cache/" -- Discogs from cache
    )

envTidalConnect :: Int -> AppM Env
envTidalConnect _nalbums = do
  -- (_, tidal, aMusic, _, _) <- liftIO getProviders
  (_, tidal, aMusic, dci, _) <- liftIO getProviders

  env <- ask
  oldAlbums <- readIORef $ albumsR env
  vta <- liftIO $ readTidalAlbums tidal
  vma <- liftIO $ readAMusicAlbums aMusic
  newFolders <- readFolders
  let tidalAlbums = M.fromList $ (\a -> (albumID a, a)) <$> V.toList vta
  let aMusicAlbums = M.fromList $ (\a -> (albumID a, a)) <$> V.toList vma
  allAlbums <- withPlays $ oldAlbums <> tidalAlbums <> aMusicAlbums
  _ <- liftIO $ writeIORef (albumsR env) allAlbums

  -- create the Tags index
  putTextLn "----------------------envTidalConnect: Updating Tags index"
  let tagsMap :: Map Text [AlbumKey]
      tagsMap = buildTags (M.elems allAlbums)
  putTextLn "----------------------envTidalConnect: list of Tags found: "
  print (M.keys tagsMap)

  -- reread Discogs lists info
  lm <- liftIO $ readDiscogsLists dci
  -- reread folder album ids
  let fm :: Map Text (Int, Vector AlbumKey)
      fm = readFolderAids newFolders allAlbums
  let allLists = lm <> fm
  _ <- M.traverseWithKey (\n (i, vi) -> putTextLn $ show n <> "--" <> show i <> ": " <> show (length vi)) allLists

  _ <- writeIORef (listsR env) allLists
  _ <- writeIORef (listNamesR env) $ M.fromList . map (\(ln, (lid, _)) -> (ln, lid)) $ M.toList allLists
  _ <- writeIORef (sortNameR env) "Default"
  _ <- writeIORef (sortOrderR env) Asc

  pure env

envUpdateMenuParams :: AppM MenuParams
envUpdateMenuParams = do
  envr <- envGetEnvr
  env <- ask

  let uhq :: Text
      uhq = url env <> "albums/"

      ss = sorts env -- sort criteria
      sts :: [Text] -- sorted tags
      sts =
        -- sorted tags
        filter (isJust . T.find ('.' ==)) (M.keys (tags envr))
          <> filter (isNothing . T.find ('.' ==)) (M.keys (tags envr))

      sli =
        -- the play log's lists first, then the sorted lists. The Discogs
        -- "YYYY Listened" lists stay loaded (reachable by URL) but are left out
        -- of the menu: "Listened YYYY" from the play log covers them.
        ("Listened" : map ("Listened " <>) (listenedYears (plays envr)))
          -- read off the collection.* tags (Provider.memberList), not stored lists
          <> ["Collection", "Inbox"]
          -- the file server: everything, and one list per storage location
          <> (if null storages then [] else "Server" : map ("Server " <>) storages)
          <> filter (\n -> not (pLocList n || discogsListenedList n)) (M.keys (listNames envr))

      storages = mapMaybe (T.stripPrefix "storage.") (M.keys (tags envr))

      discogsListenedList :: Text -> Bool
      discogsListenedList n = case words n of
        [y, "Listened"] -> T.length y == 4 && T.all (`elem` ['0' .. '9']) y
        _ -> False

      slo =
        -- sorted locations
        filter pLocList $ M.keys (listNames envr)

  let ms =
        MenuParams
          { muhq = uhq
          , msorts = ss
          , msts = sts
          , mlistNames = sli
          , mlocNames = slo
          , mjellyfin = jellyfinWeb env
          }
  pure ms

-- get laters n releases from Discogs
-- "Update from Tidal": take in the Tidal album store again after update-json --tidal has
-- brought it up to date (new favourites, the Save for later list), without a restart.
-- Tidal albums are replaced by the store's, so their tags follow; the Wikidata links and
-- the album facts are read again, since the update may have added to them; the Tidal and
-- All lists are rebuilt. Discogs is not asked. Returns how many albums are new.
envTidalReload :: AppM Int
envTidalReload = do
  env <- ask
  (_, tidalCached, _, _, _) <- liftIO getProviders
  oldAlbums <- readIORef (albumsR env)
  vta <- liftIO $ readTidalAlbums tidalCached
  liftIO readWikiStore >>= writeIORef (wikiLinksR env) . attachWiki
  liftIO readDetailStore >>= writeIORef (albumFactsR env) . factsFor
  let tidalAlbums = M.fromList $ (\a -> (albumID a, a)) <$> V.toList vta
      new = M.size (tidalAlbums `M.difference` oldAlbums)
  -- an album removed from clutter is in the store, marked, and no longer among its albums
  dropped <- liftIO readTidalDropped
  allAlbums <- withPlays (tidalAlbums <> foldr M.delete oldAlbums dropped)
  writeIORef (albumsR env) allAlbums
  writeIORef (tagsR env) (buildTags (M.elems allAlbums))
  oldLists <- readIORef (listsR env)
  let rebuilt = readFolderAids M.empty allAlbums -- the built-in lists only
      fresh = M.filterWithKey (\k _ -> k `elem` ["Tidal", "All"]) rebuilt
  writeIORef (listsR env) (fresh <> oldLists)
  pure new

-- the plays of file albums under the ids their folders have now (Collection: "moved")
movedPlays :: Moves -> [PlayEvent] -> [PlayEvent]
movedPlays mv
  | M.null mv = id
  | otherwise = map (\e -> e{peJellyfin = followMove mv <$> peJellyfin e})

-- Take clutter's Jellyfin album store in again (after scripts/update-jellyfin-albums.py),
-- without a restart: the file albums are replaced by the store's; those no longer in it
-- leave, also as versions of other albums; playing times are read again. Jellyfin gives a
-- moved folder a new id: where the script found the album again it wrote a "moved" line to
-- the collection log, so the log is read again too and the links, marks and plays of the
-- old id are the new one's. A link whose file album is gone and was not found again is left
-- dangling and counted. Returns (new, gone, moved, dangling links).
envJellyfinReload :: AppM (Int, Int, Int, Int)
envJellyfinReload = do
  env <- ask
  old <- readIORef (albumsR env)
  jstore <- liftIO readJellyfinStore
  liftIO readDetailStore >>= writeIORef (albumFactsR env) . factsFor
  (marks, links', streams, moves) <- liftIO $ collectionFile >>= readCollection
  writeIORef (marksR env) marks
  writeIORef (linksR env) links'
  writeIORef (streamsR env) streams
  modifyIORef' (playsR env) (movedPlays moves)
  let isFile a = case albumID a of
        J _ -> True
        _ -> False
      (oldFiles, others) = M.partition isFile old
      files = M.fromList [(albumID a, a) | a <- jellyfinAlbums jstore]
      there j = M.member (J j) files
      -- a file version that is gone leaves its album; attachLinks puts the rest back
      pruned = M.map (\a -> let fs = filter there (albumFiles a) in if fs == albumFiles a then a else a{albumFiles = fs, albumJellyfin = viaNonEmpty head fs}) others
      new = M.size (files `M.difference` oldFiles)
      goneIds = [j | J j <- M.keys (oldFiles `M.difference` files)]
      gone = length goneIds
      -- the albums that left and are here again under a new id
      moved = length [j | j <- goneIds, let j' = followMove moves j, j' /= j, there j']
      dangling = length (filter (not . there) (M.keys links'))
  allAlbums <- withPlays (files <> pruned)
  writeIORef (albumsR env) allAlbums
  writeIORef (tagsR env) (buildTags (M.elems allAlbums))
  pure (new, gone, moved, dangling)

-- also update folders and lists, update other metadata
envUpdate :: Maybe Text -> Maybe Text -> Int -> AppM ()
envUpdate _tok _un nreleases = do
  env <- ask

  -- save tidal albums map
  oldAlbums <- readIORef $ albumsR env
  oldLists <- readIORef $ listsR env
  let (_, tl) = fromMaybe (0, V.empty) $ M.lookup "Tidal" oldLists
      vta :: Vector Album
      vta = V.fromList $ mapMaybe (`M.lookup` oldAlbums) $ V.toList tl
      tidalAlbums = M.fromList $ (\a -> (albumID a, a)) <$> V.toList vta

  -- update with the new discogs info
  -- let discogs' = DiscogsSession (fromMaybe "" tok) (fromMaybe "" un)
  -- putTextLn $ "-----------------Updating from " <> show discogs'
  -- _ <- writeIORef (discogsR env) discogs'

  -- reread Discogs folders info
  newFolders <- readFolders

  -- reread Discogs albums info, overwriting with changes
  -- newAlbums <- if nreleases == 0
  --                   then
  --                     M.fromList . map (\a -> (albumID a, a)) . V.toList
  --                       <$> readAlbums env
  --                   else
  --                     M.fromList . map (\a -> (albumID a, a)) . V.toList
  --                       <$> readAlbums env nreleases
  newAlbums <-
    M.fromList . map (\a -> (albumID a, a)) . V.toList
      <$> readAlbums nreleases
  allAlbums <- withPlays $ newAlbums <> oldAlbums <> tidalAlbums
  _ <- writeIORef (albumsR env) allAlbums

  -- create the Tags index
  putTextLn "----------------------envUpdate: Updating Tags index"
  let tagsMap :: Map Text [AlbumKey]
      tagsMap = buildTags (M.elems allAlbums)
  putTextLn "----------------------envUpdate: list of Tags found: "
  print (M.keys tagsMap)

  -- reread Discogs lists info
  dci <- envGetDiscogs
  lm <- liftIO $ readDiscogsLists dci
  -- reread folder album ids
  let fm :: Map Text (Int, Vector AlbumKey)
      fm = readFolderAids newFolders allAlbums
  let allLists = lm <> fm
  _ <- M.traverseWithKey (\n (i, vi) -> putTextLn $ show n <> "--" <> show i <> ": " <> show (length vi)) allLists

  _ <- writeIORef (listsR env) allLists
  _ <- writeIORef (listNamesR env) $ M.fromList . map (\(ln, (lid, _)) -> (ln, lid)) $ M.toList allLists
  _ <- writeIORef (sortNameR env) "Default"
  _ <- writeIORef (sortOrderR env) Asc
  pure ()

-- create list of [ <albumID>, (locList name, locList position) ]
discogsLocList :: (Text, (Int, Vector AlbumKey)) -> [(AlbumKey, (Text, Int))]
discogsLocList (ln, (1627102, aids)) = zipWith (\idx aid -> (aid, (ln, idx))) [1 ..] (V.toList aids)
discogsLocList (ln, (_, aids)) = zipWith (\idx aid -> (aid, (ln, idx))) [1 ..] (V.toList aids)

-- the tags index for all albums in one pass; inserting album by album with L.union costs
-- n^2 per tag, deferred to the tag's first use, which made a focus on a big tag
-- (tidal.collection, provider.applemusic) take a minute
buildTags :: [Album] -> Map Text [AlbumKey]
buildTags as = M.map (ordNub . reverse) $ M.fromListWith (<>) [(t, [albumID a]) | a <- as, t <- albumTags a]

updateTags :: Album -> Map Text [AlbumKey] -> Map Text [AlbumKey]
updateTags a m =
  foldr
    (\k mm -> M.insertWith L.union k (one (albumID a)) mm)
    m
    (albumTags a)

-- initialize Env
-- get info from Tidal
-- get folder, list, release info from Discogs or cached JSON, depending on flag c
-- we do not yet have the App Monad, so this is in the IO Monad instead
--
initInit :: Bool -> IO (Discogs, Map AlbumKey Album, Map Text Int, Map Text (Int, Vector AlbumKey), Text)
initInit cached = do
  (tidal, tidalCached, aMusic, dci, dci_) <- getProviders

  let t = if cached then tidalCached else tidal
  let dc = if cached then dci_ else dci
  if cached
    then putTextLn "-----------------using cached Discogs and Tidal collection info"
    else putTextLn "-----------------reading Discogs and Tidal collection info from the web"

  -- read the map of Discogs folder names and ids
  -- fns :: Map Text Int
  fns <- readDiscogsFolders dc

  -- vda/vma/vta :: Vector of Album
  vta <- readTidalAlbums t
  vma <- readAMusicAlbums aMusic
  vda <- liftIO $ readDiscogsAlbums dc fns
  -- the albums on the file server, from clutter's Jellyfin album store
  jstore <- readJellyfinStore
  let vja = V.fromList (jellyfinAlbums jstore)

  let albums' :: Map AlbumKey Album
      albums' =
        M.fromList $
          (\a -> (albumID a, a)) <$> V.toList (vda <> vta <> vma <> vja)

  -- read the map of Discogs lists (still empty album ids if from API)
  lm <- readDiscogsLists dc

  pure (dci, albums', fns, lm, jsWeb jstore)

envInit :: Bool -> IO Env
envInit c = do
  -- set up the sort functions
  -- and populate the initial env maps from cache files
  --
  -- get Map of all albums from Providers:
  -- retrieve database from files
  --
  -- at this point, we do not have the AppM monad yet, so are working in IO
  -- get initial database info from providers and/or cached JSON
  --  dc :: Discogs                     -- discogs credentials
  --  albums' :: Map AlbumKey Album          -- map of Albums indexed with their IDs
  --  fns :: Map Text Int               -- map of folder names with their IDs
  --  lm :: Map Text (Int, Vector AlbumKey)  -- map of list names with IDs and contents
  (dc, albums'', fns, lm, jweb) <- initInit c

  -- clutter's play log, attached to the albums before the tags index is built
  ldir <- listenedDir
  plays0 <- readPlays ldir
  hPutStrLn stderr $ "read " <> show (length plays0) <> " plays from " <> ldir
  -- Wikipedia/AllMusic links (docs/wikidata-plan.md), also re-attached by withPlays
  wstore <- readWikiStore
  -- facts for the album page (docs/album-page-plan.md, phase 1)
  dstore <- readDetailStore
  -- the keep marks (docs/jellyfin-plan.md, "Membership")
  (marks', links', streams', moves') <- collectionFile >>= readCollection
  -- a play of a file album whose folder has since moved is the new id's
  let plays' = movedPlays moves' plays0
  sit <- standInFolderTags
  let albums' = attachCollection sit marks' (attachLength (factsFor dstore) (attachWiki wstore (linkTidalTags (attachPlays plays' (attachLinks links' streams' albums'')))))

  -- create the Tags index
  putTextLn "----------------------envInit: Updating Tags index"
  let tagsMap :: Map Text [AlbumKey]
      tagsMap = buildTags (M.elems albums')
  putTextLn "----------------------envInit: list of Tags found: "
  print (M.keys tagsMap)

  let fm :: Map Text (Int, Vector AlbumKey)
      fm = readFolderAids fns albums'

  let lists' = lm <> fm
  let listNames' :: Map Text Int
      listNames' = M.fromList . map (\(ln, (lid, _)) -> (ln, lid)) $ M.toList lists'
  _ <- M.traverseWithKey (\n (i, vi) -> putTextLn $ show n <> "--" <> show i <> ": " <> show (length vi)) lists'
  let lcs' = M.fromList . concatMap discogsLocList . filter (pLocList . fst) . M.toList $ lm
  putTextLn "----------------------envInit: first 10 locs: "
  print . take 10 . M.toList $ lcs'

  dr <- newIORef dc
  ar <- newIORef albums'
  lr <- newIORef lists'
  lcs <- newIORef lcs'
  lds <- newIORef plays'
  lnr <- newIORef listNames'
  sr <- newIORef "Default"
  so <- newIORef Asc
  tr <- newIORef tagsMap
  fr <- newIORef []
  wlr <- newIORef (attachWiki wstore)
  afr <- newIORef (factsFor dstore)
  mkr <- newIORef marks'
  lkr <- newIORef links'
  skr <- newIORef streams'
  -- define sort functions and map to names
  let sDef :: Map AlbumKey Album -> SortOrder -> Vector AlbumKey -> Vector AlbumKey
      sDef _ s l = case s of
        Asc -> l
        _ -> V.reverse l
  let sortAsi :: Map AlbumKey Album -> Vector AlbumKey -> [(AlbumKey, Maybe Album)]
      sortAsi am = map (\aid -> (aid, M.lookup aid am)) . V.toList
      compareAsc f (_, a) (_, b) = comparing f a b
      compareDesc f (_, a) (_, b) = comparing f b a
  let sTitle :: Map AlbumKey Album -> SortOrder -> Vector AlbumKey -> Vector AlbumKey
      sTitle am s aids = V.fromList (fst <$> sortBy (comp s) (sortAsi am aids))
       where
        comp o = case o of
          Asc -> compareAsc (fmap albumTitle)
          Desc -> compareDesc (fmap albumTitle)
  let sArtist :: Map AlbumKey Album -> SortOrder -> Vector AlbumKey -> Vector AlbumKey
      sArtist am s aids = V.fromList (fst <$> sortBy (comp s) (sortAsi am aids))
       where
        comp o = case o of
          Asc -> compareAsc (fmap albumArtist)
          Desc -> compareDesc (fmap albumArtist)
  let sAdded :: Map AlbumKey Album -> SortOrder -> Vector AlbumKey -> Vector AlbumKey
      sAdded am s aids = V.fromList (fst <$> sortBy (comp s) (sortAsi am aids))
       where
        comp o = case o of
          Asc -> compareDesc (fmap albumAdded)
          Desc -> compareAsc (fmap albumAdded)
  let sfs :: Map Text (Map AlbumKey Album -> SortOrder -> Vector AlbumKey -> Vector AlbumKey) -- sort functions
      sfs =
        M.fromList
          [ ("Default", sDef)
          , ("Artist", sArtist)
          , ("Title", sTitle)
          , ("Added", sAdded)
          ]
  pure
    Env
      { discogsR = dr
      , albumsR = ar
      , listsR = lr
      , locsR = lcs
      , playsR = lds
      , listNamesR = lnr
      , tagsR = tr
      , focusR = fr
      , sortNameR = sr
      , sortOrderR = so
      , url = "/"
      , getList = getList'
      , sorts = V.fromList $ M.keys sfs
      , getSort = \am sn -> fromMaybe sDef (M.lookup sn sfs) am
      , wikiLinksR = wlr
      , albumFactsR = afr
      , jellyfinWeb = jweb
      , marksR = mkr
      , linksR = lkr
      , streamsR = skr
      , standInTags = sit
      }

--
-- define the function for (env getList) :: Text -> AppM ( Vector Int )
-- that will evaluate the list of Album IDs for List name
--  if list in Env is empty, try to get from provider
getList' :: Text -> AppM (Vector AlbumKey)
getList' ln = case listenedName ln of
  -- one entry per play, newest first: an album played twice is listed twice
  Just my -> do
    env <- ask
    es <- readIORef (playsR env)
    am <- readIORef (albumsR env)
    pure . V.fromList . mapMaybe (fmap albumID . snd) $ listenedRows my es am
  Nothing
    | ln == "Collection" -> memberList "collection.in" <$> (asks albumsR >>= readIORef)
    | ln == "Inbox" -> memberList "collection.inbox" <$> (asks albumsR >>= readIORef)
    | ln == "All" -> allList <$> (asks albumsR >>= readIORef)
    | ln == "Server" -> serverList Nothing <$> (asks albumsR >>= readIORef)
    | Just s <- T.stripPrefix "Server " ln -> serverList (Just s) <$> (asks albumsR >>= readIORef)
    | otherwise -> getDiscogsList ln

getDiscogsList :: Text -> AppM (Vector AlbumKey)
getDiscogsList ln = do
  env <- ask
  ls' <- readIORef (listsR env)
  let (lid, aids') = fromMaybe (0, V.empty) (M.lookup ln ls')
  if V.null aids'
    then do
      aids <- readDiscogsListAids lid
      -- update location info in albums
      --   go through this list and update location in albums
      -- am <- liftIO ( readIORef (albumsR env) )
      -- am' <- updateLocations lists ln am aids -- not yet implemented
      -- _ <- writeIORef (albumsR env) am'
      when (pLocList ln) $ do
        lcs <- readIORef (locsR env)
        let lcs' = M.union (M.fromList (discogsLocList (ln, (lid, aids)))) lcs
        _ <- writeIORef (locsR env) lcs'
        pure ()
      -- write back modified lists
      _ <- writeIORef (listsR env) $ M.insert ln (lid, aids) ls'
      pure aids
    else pure aids'

-- The album page's album, at once: as clutter holds it (slice 18 — no Discogs call, no
-- recomputation). Only a Discogs release clutter does not hold yet is fetched.
envGetAlbum :: AlbumKey -> AppM (Maybe Album)
envGetAlbum aid =
  (M.lookup aid <$> (asks albumsR >>= readIORef)) >>= \case
    Just a -> pure (Just a)
    Nothing -> case aid of
      D _ -> envUpdateAlbum aid
      _ -> pure Nothing

-- After the page is drawn: ask Discogs whether a release has changed (rating, folder,
-- notes, title …). Just the album as it is now if it has -- taken in with envUpdateAlbum's
-- full update --, Nothing if not, or if it is no Discogs release or Discogs cannot be asked.
envRefreshAlbum :: AlbumKey -> AppM (Maybe Album)
envRefreshAlbum aid = do
  dc <- asks discogsR >>= readIORef
  held <- M.lookup aid <$> (asks albumsR >>= readIORef)
  case (aid, dc) of
    (D i, DiscogsSession _ _) ->
      readAlbum i >>= \case
        Just fresh | maybe True (\h -> discogsView h /= discogsView fresh) held -> do
          liftIO $ hPutStrLn stderr ("albumrefresh " <> show i <> ": changed on Discogs, updating")
          envPutAlbum fresh
          M.lookup aid <$> (asks albumsR >>= readIORef)
        Just _ -> liftIO (hPutStrLn stderr ("albumrefresh " <> show i <> ": unchanged")) $> Nothing
        Nothing -> liftIO (hPutStrLn stderr ("albumrefresh " <> show i <> ": Discogs did not answer")) $> Nothing
    _ -> pure Nothing

-- what a Discogs release brings with it, without what clutter attaches (plays, links,
-- collection, lengths, …: tags with those prefixes, and the other fields)
discogsView :: Album -> (Text, Text, Text, Text, Text, Int, Text, Maybe Int, Maybe Text, Int, Int, [Text])
discogsView a =
  ( albumTitle a
  , albumArtist a
  , albumReleased a
  , albumCover a
  , albumAdded a
  , albumFolder a
  , albumFormat a
  , albumLocIdx a
  , albumLocation a
  , albumRating a
  , albumPlays a
  , sort (filter own (albumTags a))
  )
 where
  own t = not (any (`TX.isPrefixOf` t) ["played.", "collection.", "keep.", "length.", "provider.", "storage.", "tidal."])

-- A Discogs release, fetched again from Discogs, taken in: the album map, all tags, the
-- Discogs folders and lists (several Discogs calls). Used for a release clutter does not
-- hold, when Discogs has changed one, and by the old server-rendered pages.
envUpdateAlbum :: AlbumKey -> AppM (Maybe Album)
envUpdateAlbum aid = do
  env <- ask
  dc <- readIORef (discogsR env)
  am' <- readIORef (albumsR env)
  -- check if this release id is already known / in the Map
  let ma' :: Maybe Album
      ma' = M.lookup aid am'
  -- a Discogs release is updated from Discogs; Tidal, Apple Music, Jellyfin albums are taken as held
  ma <- case (aid, dc) of
    (D i, DiscogsSession _ _) -> readAlbum i
    (D _, _) -> pure Nothing -- we only have the caching files
    _ -> pure ma'
  whenJust ma envPutAlbum
  -- the stored album, which carries its play dates
  ma'' <- case ma of
    Just a -> Just . fromMaybe a . M.lookup aid <$> readIORef (albumsR env)
    Nothing -> pure Nothing
  putTextLn "----------------envUpdateAlbum-----------------------------"
  print ma''
  pure ma''

envPutAlbum :: Album -> AppM ()
envPutAlbum a = do
  env <- ask
  dc <- readIORef (discogsR env)
  am' <- readIORef (albumsR env)
  ls <- readIORef (listsR env)
  let aid = albumID a
  -- insert updated album and put in album map
  am <- withPlays $ M.insert aid a am'
  liftIO $ writeIORef (albumsR env) am
  -- insert aid into its folder
  -- let folder = albumFolder a
  -- update folder "lists" and invalidate lists
  -- also update Tidal and AMusic "special" list
  liftIO $ writeIORef (listsR env) (updateTidalFolderAids am ls)
  liftIO $ writeIORef (listsR env) (updateAMusicFolderAids am ls)

  -- update the tags map with tags from this album
  tm <- readIORef (tagsR env)
  _ <- writeIORef (tagsR env) (updateTags (fromMaybe a (M.lookup aid am)) tm)

  -- update lists and folders
  newFolders <- readFolders
  -- reread Discogs lists info
  lm <- liftIO $ readDiscogsLists dc
  -- reread folder album ids
  let fm :: Map Text (Int, Vector AlbumKey)
      fm = readFolderAids newFolders am
  let allLists = lm <> fm
  -- _ <- M.traverseWithKey (\n (i, vi) -> putTextLn $ show n <> "--" <> show i <> ": " <> show (length vi)) allLists

  _ <- writeIORef (listsR env) allLists
  writeIORef (listNamesR env) $ M.fromList . map (\(ln, (lid, _)) -> (ln, lid)) $ M.toList allLists

envGetTag :: Text -> AppM [AlbumKey]
envGetTag t = do
  tm <- asks tagsR >>= readIORef
  pure $ fromMaybe [] (M.lookup t tm)

envUpdateSort :: Maybe Text -> Maybe Text -> AppM EnvR
envUpdateSort msb mso = do
  env <- ask
  am <- readIORef (albumsR env)
  lns <- readIORef (listNamesR env)
  lm <- readIORef (listsR env)
  lcs <- readIORef (locsR env)
  dc <- readIORef (discogsR env)
  tm <- readIORef (tagsR env)
  fs <- readIORef (focusR env)
  sn <- case msb of
    Nothing -> readIORef (sortNameR env)
    Just sb -> do
      _ <- writeIORef (sortNameR env) sb
      pure sb
  so <- case mso of
    Nothing -> readIORef (sortOrderR env)
    Just sot -> do
      let so = case sot of
            "Desc" -> Desc
            _ -> Asc
      _ <- writeIORef (sortOrderR env) so
      pure so
  lds <- readIORef (playsR env)
  pure $ EnvR am lm lcs lds lns sn so dc tm fs

-- albumListenedDates for an album map, from the play log in Env
withPlays :: Map AlbumKey Album -> AppM (Map AlbumKey Album)
withPlays am = do
  es <- asks playsR >>= readIORef
  wl <- asks wikiLinksR >>= readIORef
  -- the length.* tags, from the album facts as last read
  fa <- asks albumFactsR >>= readIORef
  -- the collection.* tags, from the keep marks and the plays
  marks <- asks marksR >>= readIORef
  sit <- asks standInTags
  links <- asks linksR >>= readIORef
  streams <- asks streamsR >>= readIORef
  pure (attachCollection sit marks (attachLength fa (wl (linkTidalTags (attachPlays es (attachLinks links streams am))))))

-- set or take off the keep mark of an album (the log line is written by the caller), and
-- bring the collection.* tags up to date
envSetMark :: AlbumKey -> Maybe Bool -> AppM ()
envSetMark k m = do
  mr <- asks marksR
  modifyIORef' mr (M.alter (const m) k)
  envRefreshPlays

-- the same for many albums at once (bulk Keep in a list): one recomputation of the tags
envSetMarks :: [AlbumKey] -> Maybe Bool -> AppM ()
envSetMarks ks m = do
  mr <- asks marksR
  modifyIORef' mr (\ms -> foldl' (flip (M.alter (const m))) ms ks)
  envRefreshPlays

-- make a file album a version of an album (Just), or take the link off (Nothing); the log
-- line is written by the caller. The file album leaves the album it was on before.
envSetLink :: Text -> Maybe AlbumKey -> AppM ()
envSetLink j mk = do
  lr <- asks linksR
  ar <- asks albumsR
  modifyIORef' lr (M.alter (const mk) j)
  let off a = case albumID a of
        J _ -> a
        _ | j `elem` albumFiles a -> let fs = filter (/= j) (albumFiles a) in a{albumFiles = fs, albumJellyfin = viaNonEmpty head fs}
        _ -> a
  modifyIORef' ar (M.map off)
  envRefreshPlays

-- give a release a stream (True) or take it off (False); the log line is written by the
-- caller. A stream of the same kind that the release had is replaced.
envSetStream :: AlbumKey -> AlbumKey -> Bool -> AppM ()
envSetStream k s linked = do
  sr <- asks streamsR
  ar <- asks albumsR
  old <- readIORef sr
  let had = M.findWithDefault [] k old
      gone = if linked then filter (sameKind s) had else [s]
      now' = (if linked then (s :) else id) (filter (`notElem` gone) had)
      clear st a = case st of
        T n | albumTidal a == Just (show n) -> a{albumTidal = Nothing, albumTags = filter (/= "provider.tidal") (albumTags a), albumWikiKeys = filter (/= ("t:" <> show n)) (albumWikiKeys a)}
        A n | albumAMusic a == Just (show n) -> a{albumAMusic = Nothing, albumTags = filter (/= "provider.applemusic") (albumTags a), albumWikiKeys = filter (/= ("a:" <> show n)) (albumWikiKeys a)}
        Q q | albumQobuz a == Just q -> a{albumQobuz = Nothing, albumTags = filter (/= "provider.qobuz") (albumTags a)}
        _ -> a
  writeIORef sr (if null now' then M.delete k old else M.insert k now' old)
  modifyIORef' ar (M.adjust (\a -> foldr clear a gone) k)
  envRefreshPlays

-- re-attach the play dates to all albums, after a play was logged
envRefreshPlays :: AppM ()
envRefreshPlays = do
  ar <- asks albumsR
  am <- readIORef ar >>= withPlays
  writeIORef ar am
  -- played.* tags change with a play: rebuild the index (a few thousand albums)
  tr <- asks tagsR
  writeIORef tr (buildTags (M.elems am))
