-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Env (
  envUpdate,
  envInit,
  envUpdateAlbum,
  envTidalConnect,
  envGetTag,
  envUpdateSort,
  envUpdateMenuParams,
)
where

import qualified Data.List as L (
  union,
 )
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Vector (Vector)
import qualified Data.Vector as V (
  empty,
  fromList,
  null,
  reverse,
  toList,
 )
import Provider (
  readAMusicAlbums,
  readAlbum,
  readAlbums,
  readDiscogsAlbums,
  readDiscogsFolders,
  readDiscogsLists,
  readFolderAids,
  readFolders,
  readListAids,
  readLists,
  readTidalAlbums,
  updateAMusicFolderAids,
  updateTidalFolderAids,
 )

import Relude
import Types (
  -- Discogs (..),

  AMusic (..),
  AMusicInfo (..),
  Album (..),
  AppM,
  Discogs (..),
  Env (..),
  EnvR (..),
  MenuParams (..),
  SortOrder (..),
  Tidal (..),
  TidalInfo (..),
  envGetEnvr,
  pLocList,
 )

import qualified Data.Text as T (find)

-- get authorization info from a text file
getProviders :: IO (Tidal, Tidal, AMusic, Discogs, Discogs)
getProviders = do
  t <- readFileBS "tok.dat" -- for debug, get from file with authentication data
  let ts :: [Text]
      ts = words . decodeUtf8 $ t
      appleMusicDevToken = fromJust $ ts !!? 0 -- t6
      appleMusicUserToken = fromJust $ ts !!? 1 -- t7
      discogsDevToken = fromJust $ ts !!? 2
      discogsUserName = fromJust $ ts !!? 3
      tidalUserId = fromMaybe 0 $ readMaybe (toString . fromJust $ ts !!? 4) :: Int
      tidalSessionId = fromJust $ ts !!? 5 -- t3
      tidalCountryCode = fromJust $ ts !!? 6 -- t4
      tidalAccessToken = fromJust $ ts !!? 7 -- t5
  pure
    ( Tidal $ TidalSession tidalUserId tidalSessionId tidalCountryCode tidalAccessToken
    , Tidal $ TidalFile "cache/traw.json" -- Tidal from cache
    , AMusic $ AMusicSession appleMusicDevToken appleMusicUserToken
    , DiscogsSession discogsDevToken discogsUserName
    , DiscogsFile "cache/" -- Discogs from cache
    )

envTidalConnect :: Int -> AppM Env
envTidalConnect _nalbums = do
  (tidal, _, aMusic, _, _) <- liftIO getProviders

  env <- ask
  oldAlbums <- readIORef $ albumsR env
  vta <- liftIO $ readTidalAlbums tidal
  vma <- liftIO $ readAMusicAlbums aMusic
  newFolders <- readFolders
  let tidalAlbums = M.fromList $ (\a -> (albumID a, a)) <$> V.toList vta
  let aMusicAlbums = M.fromList $ (\a -> (albumID a, a)) <$> V.toList vma
  let allAlbums = oldAlbums <> tidalAlbums <> aMusicAlbums
  _ <- liftIO $ writeIORef (albumsR env) allAlbums

  -- create the Tags index
  putTextLn "-------------- Updating Tags index"
  let tagsMap :: Map Text [Int]
      tagsMap = foldr updateTags M.empty (M.elems allAlbums)
  putTextLn "---------------------- list of Tags found: "
  print (M.keys tagsMap)

  -- reread Discogs lists info
  lm <- readLists
  -- reread folder album ids
  let fm :: Map Text (Int, Vector Int)
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

      ss = sorts env
      -- [ "Added", "Artist", "Default", "Title" ]

      sts :: [Text] -- sorted tags
      sts =
        filter (isJust . T.find ('.' ==)) (M.keys (tags envr))
          <> filter (isNothing . T.find ('.' ==)) (M.keys (tags envr))
      -- [ "folder.cd", "folder.pop", "genre.classical", "genre.opera", "rated.*****" ] -- sorted tags

      sli = filter (not . pLocList) $ M.keys (listNames envr)
      --  [ "2020 Listened", "2021 Listened", "2022 Listened", "All", "Apple Music",  "Discogs", "Pop", "Tidal" ]

      slo = filter pLocList $ M.keys (listNames envr)
  -- [ "Cube A0", "Cube B0 Pop", "Cube E0 Incoming", "Cube E1 Incoming", "Shelf A1 Opera" ]

  let ms =
        MenuParams
          { muhq = uhq
          , msorts = ss
          , msts = sts
          , mlistNames = sli
          , mlocNames = slo
          }
  pure ms

-- get laters n releases from Discogs
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
  let allAlbums = newAlbums <> oldAlbums <> tidalAlbums
  _ <- writeIORef (albumsR env) allAlbums

  -- create the Tags index
  putTextLn "-------------- Updating Tags index"
  let tagsMap :: Map Text [Int]
      tagsMap = foldr updateTags M.empty (M.elems allAlbums)
  putTextLn "---------------------- list of Tags found: "
  print (M.keys tagsMap)

  -- reread Discogs lists info
  lm <- readLists
  -- reread folder album ids
  let fm :: Map Text (Int, Vector Int)
      fm = readFolderAids newFolders allAlbums
  let allLists = lm <> fm
  _ <- M.traverseWithKey (\n (i, vi) -> putTextLn $ show n <> "--" <> show i <> ": " <> show (length vi)) allLists

  _ <- writeIORef (listsR env) allLists
  _ <- writeIORef (listNamesR env) $ M.fromList . map (\(ln, (lid, _)) -> (ln, lid)) $ M.toList allLists
  _ <- writeIORef (sortNameR env) "Default"
  _ <- writeIORef (sortOrderR env) Asc
  pure ()

fromListMap :: (Text, (Int, Vector Int)) -> [(Int, (Text, Int))]
fromListMap (ln, (_, aids)) = zipWith (\idx aid -> (aid, (ln, idx))) [1 ..] (V.toList aids)

updateTags :: Album -> Map Text [Int] -> Map Text [Int]
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
initInit :: Bool -> IO (Discogs, Map Int Album, Map Text Int, Map Text (Int, Vector Int))
initInit c = do
  (tidal, _tidal, aMusic, dci, dci_) <- getProviders

  let dc = if c then dci_ else dci
  if c
    then putTextLn "-----------------using cached Discogs collection info"
    else putTextLn "-----------------reading Discogs collection info from the web"

  -- read the map of Discogs folder names and ids
  -- fns :: Map Text Int
  fns <- readDiscogsFolders dc

  -- vda/vma/vta :: Vector of Album
  vta <- readTidalAlbums tidal
  vma <- readAMusicAlbums aMusic
  vda <- liftIO $ readDiscogsAlbums dc fns

  let albums' :: Map Int Album
      albums' =
        M.fromList $
          (\a -> (albumID a, a)) <$> V.toList (vda <> vta <> vma)

  -- create the Tags index
  putTextLn "-----------------Updating Tags index"
  let tagsMap :: Map Text [Int]
      tagsMap = foldr updateTags M.empty (M.elems albums')
  putTextLn "---------------------- list of Tags found: "
  print (M.keys tagsMap)

  -- read the map of Discogs lists (still empty album ids if from API)
  lm <- readDiscogsLists dc

  pure (dci, albums', fns, lm)

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
  --  albums' :: Map Int Album          -- map of Albums indexed with their IDs
  --  fns :: Map Text Int               -- map of folder names with their IDs
  --  lm :: Map Text (Int, Vector Int)  -- map of list names with IDs and contents
  (dc, albums', fns, lm) <- initInit c

  -- create the Tags index
  putTextLn "-------------- Updating Tags index"
  let tagsMap :: Map Text [Int]
      tagsMap = foldr updateTags M.empty (M.elems albums')

  let fm :: Map Text (Int, Vector Int)
      fm = readFolderAids fns albums'

  let lists' = lm <> fm
  let listNames' :: Map Text Int
      listNames' = M.fromList . map (\(ln, (lid, _)) -> (ln, lid)) $ M.toList lists'
  _ <- M.traverseWithKey (\n (i, vi) -> putTextLn $ show n <> "--" <> show i <> ": " <> show (length vi)) lists'
  let allLocs = M.fromList . concatMap fromListMap . filter (pLocList . fst) . M.toList $ lm

  dr <- newIORef dc
  ar <- newIORef albums'
  lr <- newIORef lists'
  lo <- newIORef allLocs
  lnr <- newIORef listNames'
  sr <- newIORef "Default"
  so <- newIORef Asc
  tr <- newIORef tagsMap
  fr <- newIORef []
  -- define sort functions and map to names
  let sDef :: Map Int Album -> SortOrder -> Vector Int -> Vector Int
      sDef _ s l = case s of
        Asc -> l
        _ -> V.reverse l
  let sortAsi :: Map Int Album -> Vector Int -> [(Int, Maybe Album)]
      sortAsi am = map (\aid -> (aid, M.lookup aid am)) . V.toList
      compareAsc f (_, a) (_, b) = comparing f a b
      compareDesc f (_, a) (_, b) = comparing f b a
  let sTitle :: Map Int Album -> SortOrder -> Vector Int -> Vector Int
      sTitle am s aids = V.fromList (fst <$> sortBy (comp s) (sortAsi am aids))
       where
        comp o = case o of
          Asc -> compareAsc (fmap albumTitle)
          Desc -> compareDesc (fmap albumTitle)
  let sArtist :: Map Int Album -> SortOrder -> Vector Int -> Vector Int
      sArtist am s aids = V.fromList (fst <$> sortBy (comp s) (sortAsi am aids))
       where
        comp o = case o of
          Asc -> compareAsc (fmap albumArtist)
          Desc -> compareDesc (fmap albumArtist)
  let sAdded :: Map Int Album -> SortOrder -> Vector Int -> Vector Int
      sAdded am s aids = V.fromList (fst <$> sortBy (comp s) (sortAsi am aids))
       where
        comp o = case o of
          Asc -> compareDesc (fmap albumAdded)
          Desc -> compareAsc (fmap albumAdded)
  let sfs :: Map Text (Map Int Album -> SortOrder -> Vector Int -> Vector Int) -- sort functions
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
      , locsR = lo
      , listNamesR = lnr
      , tagsR = tr
      , focusR = fr
      , sortNameR = sr
      , sortOrderR = so
      , url = "/"
      , getList = getList'
      , sorts = V.fromList $ M.keys sfs
      , getSort = \am sn -> fromMaybe sDef (M.lookup sn sfs) am
      }

--
-- define the function for (env getList) :: Text -> AppM ( Vector Int )
-- that will evaluate the list of Album IDs for List name
--  if list in Env is empty, try to get from provider
getList' :: Text -> AppM (Vector Int)
getList' ln = do
  env <- ask
  ls' <- readIORef (listsR env)
  let (lid, aids') = fromMaybe (0, V.empty) (M.lookup ln ls')
  if V.null aids'
    then do
      aids <- readListAids lid
      -- update location info in albums
      --   go through this list and update location in albums
      -- am <- liftIO ( readIORef (albumsR env) )
      -- am' <- updateLocations lists ln am aids -- not yet implemented
      -- _ <- writeIORef (albumsR env) am'
      when (pLocList ln) $ do
        lcs <- readIORef (locsR env)
        let lcs' = M.union (M.fromList (fromListMap (ln, (lid, aids)))) lcs
        _ <- writeIORef (locsR env) lcs'
        pure ()
      -- write back modified lists
      _ <- writeIORef (listsR env) $ M.insert ln (lid, aids) ls'
      pure aids
    else pure aids'

envUpdateAlbum :: Int -> AppM (Maybe Album)
envUpdateAlbum aid = do
  env <- ask
  dc <- readIORef (discogsR env)
  am' <- readIORef (albumsR env)
  ls <- readIORef (listsR env)
  -- check if this release id is already known / in the Map
  let ma' :: Maybe Album
      ma' = M.lookup aid am'
  -- if it's not a Tidal or AppleMusic album, update album info from Discogs
  ma <- case fmap albumFormat ma' of
    Just "AppleMusic" -> pure ma'
    Just "Tidal" -> pure ma'
    -- Just _ -> pure ma' -- already known, nothing do add
    _ -> case dc of
      DiscogsSession _ _ -> readAlbum aid
      _ -> liftIO (pure Nothing) -- we only have the caching files
  _ <- case ma of
    Just a -> do
      -- insert updated album and put in album map
      let am = M.insert aid a am'
      liftIO $ writeIORef (albumsR env) am
      -- insert aid into its folder
      -- let folder = albumFolder a
      -- update folder "lists" and invalidate lists
      -- also update Tidal and AMusic "special" list
      liftIO $ writeIORef (listsR env) (updateTidalFolderAids am ls)
      liftIO $ writeIORef (listsR env) (updateAMusicFolderAids am ls)

      -- update the tags map with tags from this album
      tm <- readIORef (tagsR env)
      _ <- writeIORef (tagsR env) (updateTags a tm)

      -- update lists and folders
      newFolders <- readFolders
      -- reread Discogs lists info
      lm <- readLists
      -- reread folder album ids
      let fm :: Map Text (Int, Vector Int)
          fm = readFolderAids newFolders am
      let allLists = lm <> fm
      -- _ <- M.traverseWithKey (\n (i, vi) -> putTextLn $ show n <> "--" <> show i <> ": " <> show (length vi)) allLists

      _ <- writeIORef (listsR env) allLists
      _ <- writeIORef (listNamesR env) $ M.fromList . map (\(ln, (lid, _)) -> (ln, lid)) $ M.toList allLists
      pure ()
    Nothing -> pure ()
  -- let mAlbum = M.lookup aid am
  print ma
  pure ma

envGetTag :: Text -> AppM [Int]
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
  pure $ EnvR am lm lcs lns sn so dc tm fs
