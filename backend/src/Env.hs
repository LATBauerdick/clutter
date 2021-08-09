{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Env
  ( envUpdate,
    envInit,
    envUpdateAlbum,
    envGetEnvr,
    envGetTag,
    envUpdateSort,
  )
where

-- import Data.List (sortBy)
-- import Data.Ord (comparing)
import qualified Data.Map.Strict as M
import Data.Vector (Vector)
import qualified Data.Vector as V
  ( empty,
    fromList,
    null,
    reverse,
    toList,
    singleton,
    (++),
  )
import Provider
  ( readAlbum,
    readAlbums,
    readTidalAlbums,
    readFolderAids,
    readFolders,
    readListAids,
    readLists,
    rereadLists,
    updateTidalFolderAids,
  )
import Relude
import Types
  ( Album (..),
    Discogs (..),
    DiscogsInfo (..),
    Env (..),
    EnvR (..),
    SortOrder (..),
    Tidal (..),
    TidalInfo (..),
    pLocList,
  )

-- testAlbum :: Album
-- testAlbum = Album 123123
--                   "Test Title"
--                   "Test Artists"
--                   "2021"
--                   "https://img.discogs.com/cOcoe8orblZUZlh_L68I8Kx3lnA=/fit-in/600x617/filters:strip_icc():format(jpeg):mode_rgb():quality(90)/discogs-images/R-6420873-1603309252-4033.jpeg.jpg"
--                   "2021-01-01T01:23:01-07:00" 1349997 ( const "xxx" )

envInit :: IO Env
envInit = envGet Nothing Nothing

envUpdate :: Env -> Text -> Text -> IO Env
envUpdate env tok un = envGet (Just env) (Just (Discogs $ DiscogsSession tok un))

-- initialize env: if not exists yet, get from file, otherwise from Discogs
envGet :: Maybe Env -> Maybe Discogs -> IO Env
envGet _ Nothing = envFromFiles
envGet Nothing _ = envFromFiles
envGet (Just env) (Just discogs') = do
  putTextLn $ "-----------------Updating from " <> show discogs'

  -- save tidal albums map
  oldAlbums <- readIORef $ albumsR env
  oldLists <- readIORef $ listsR env
  let (_, tl) = fromMaybe (0, V.empty) $ M.lookup "Tidal" oldLists
      vta :: Vector Album
      vta = V.fromList $ mapMaybe (`M.lookup` oldAlbums) $ V.toList tl
      tidalAlbums = M.fromList $ (\a -> (albumID a, a)) <$> V.toList vta

  -- update with the new discogs info
  _ <- writeIORef (discogsR env) discogs'

  -- reread Discogs folders info
  newFolders <- readFolders env -- readDiscogsFolders

  -- reread Discogs albums info, overwriting changes

  vda <- readAlbums env
  let newAlbums :: Map Int Album
      newAlbums = M.fromList $ (\a -> (albumID a, a)) <$> V.toList vda
  let allAlbums = newAlbums <> tidalAlbums
  _ <- writeIORef (albumsR env) allAlbums

  -- create the Tags index
  putTextLn "-------------- Updating Tags index"
  let tagsMap :: Map Text (Vector Int)
      tagsMap = foldr updateTags M.empty (M.elems allAlbums)
  putTextLn "---------------------- list of Tags found: "
  print (M.keys tagsMap)

  -- reread Discogs lists info
  lm <- rereadLists env
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

fromListMap :: (Text, (Int, Vector Int)) -> [(Int, (Text, Int))]
fromListMap (ln, (_, aids)) = zipWith (\idx aid -> (aid, (ln, idx))) [1 ..] (V.toList aids)

updateTags :: Album -> Map Text (Vector Int) -> Map Text (Vector Int)
updateTags a m = foldr
            (\k mm -> M.insertWith (V.++) k (V.singleton (albumID a)) mm)
            m
            (albumTags a)

envFromFiles :: IO Env
envFromFiles = do
  putTextLn "-------------envFromFiles------------------"

  -- define the function for (env getList) :: Env -> Text -> IO ( Vector Int )
  -- that will evaluate the list of Album IDs for List name
  --  if list in Env is empty, try to get from provider
  let getList' :: Env -> Text -> IO (Vector Int)
      getList' env ln = do
        lists' <- readIORef (listsR env)
        let (lid, aids') = fromMaybe (0, V.empty) (M.lookup ln lists')
        if V.null aids'
          then do
            aids <- readListAids env lid
            -- update location info in albums
            --   go through this list and update location in albums
            -- am <- liftIO ( readIORef (albumsR env) )
            -- am' <- updateLocations lists ln am aids -- not yet implemented
            -- _ <- writeIORef (albumsR env) am'
            if pLocList ln
              then do
                lcs <- readIORef (locsR env)
                let lcs' = M.union (M.fromList (fromListMap (ln, (lid, aids)))) lcs
                _ <- writeIORef (locsR env) lcs'
                pure ()
              else pure ()
            -- write back modified lists
            _ <- writeIORef (listsR env) $ M.insert ln (lid, aids) lists'
            pure aids
          else pure aids'

  --
  -- get Map of all albums from Providers:
  -- retrieve database from files
  --
  -- debug: get web credentials etc
  t <- readFileText "data/tok.dat" -- for debug, get from file with authentication data
  let [t0, t1, t2, t3, t4, t5] = words t
      countryCode = t4
      sessionId = t3
      userId = fromMaybe 0 $ readMaybe (toString t2) :: Int
      discogsToken = t0
      discogsUser = t1
      accessToken = t5

  -- from cache file or from Tidal API
  let _tidal = Tidal $ TidalFile "data/traw2.json"
  let tidal = Tidal $ TidalSession userId sessionId countryCode accessToken
  -- from cache file or from Discogs API
  let dci = Discogs $ DiscogsFile "data/"
  let _dci = Discogs $ DiscogsSession discogsToken discogsUser

  -- initial env only needs discogs info
  dcir <- newIORef dci
  let env = Env { discogsR = dcir }

  -- read the map of Discogs folder names and ids
  -- fns :: Map Text Int
  fns <- readFolders env

  -- vda/vta :: Vector of Album
  vta <- readTidalAlbums tidal
  vda <- readAlbums env

  let albums' :: Map Int Album
      albums' =
        M.fromList $
          (\a -> (albumID a, a)) <$> V.toList (vda <> vta)

  -- create the Tags index
  putTextLn "-------------- Updating Tags index"
  let tagsMap :: Map Text (Vector Int)
      tagsMap = foldr updateTags M.empty (M.elems albums')
  putTextLn "---------------------- list of Tags found: "
  print (M.keys tagsMap)


  -- read the map of Discogs lists (still empty album ids)
  lm <- readLists env

  let fm :: Map Text (Int, Vector Int)
      fm = readFolderAids fns albums'

  let lists' = lm <> fm
  let listNames' :: Map Text Int
      listNames' =  M.fromList . map (\(ln, (lid, _)) -> (ln, lid)) $ M.toList lists'
  _ <- M.traverseWithKey (\n (i, vi) -> putTextLn $ show n <> "--" <> show i <> ": " <> show (length vi)) lists'
  let allLocs = M.fromList . concatMap fromListMap . filter (pLocList . fst) . M.toList $ lm

  -- store DiscogsSession after reading from cache files
  let dc = Discogs $ DiscogsSession discogsToken discogsUser

  dr <- newIORef dc
  ar <- newIORef albums'
  lr <- newIORef lists'
  lo <- newIORef allLocs
  lnr <- newIORef listNames'
  sr <- newIORef "Default"
  so <- newIORef Asc
  tr <- newIORef tagsMap
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
          [ ("Default", sDef),
            ("Artist", sArtist),
            ("Title", sTitle),
            ("Added", sAdded)
          ]
      -- getSort' :: Map Int Album -> Text -> (SortOrder -> Vector Int -> Vector Int)
      -- getSort' am sn = fromMaybe sDef (M.lookup sn sfs) am
      -- sorts' :: Vector Text
      -- sorts' = V.fromList $ M.keys sfs
  pure
    Env
      { discogsR = dr,
        albumsR = ar,
        listsR = lr,
        locsR = lo,
        listNamesR = lnr,
        tagsR = tr,
        sortNameR = sr,
        sortOrderR = so,
        url = "/",
        getList = getList',
        sorts = V.fromList $ M.keys sfs,
        getSort = \ am sn -> fromMaybe sDef (M.lookup sn sfs) am
      }

envUpdateAlbum :: Env -> Int -> IO ( Maybe Album )
envUpdateAlbum env aid = do
  di <- liftIO (readIORef (discogsR env))
  am' <- liftIO (readIORef (albumsR env))
  ls <- liftIO (readIORef (listsR env))
  -- check if this release id is already known / in the Map
  let ma' :: Maybe Album
      ma' = M.lookup aid am'
  -- if it's not a Tidal album, update album info from Discogs
  ma <- case fmap albumFormat ma' of
    Just "Tidal" -> pure ma'
    -- Just _ -> pure ma' -- already known, nothing do add
    _ -> case getDiscogs di of
              DiscogsSession _ _ -> liftIO (readAlbum env aid)
              _ -> liftIO (pure Nothing) -- we only have the caching files
  _ <- case ma of
    Just a -> do
      -- insert updated album and put in album map
      let am = M.insert aid a am'
      liftIO $ writeIORef (albumsR env) am
      -- insert aid into its folder
      let folder = albumFolder a
      -- update folder "lists" and invalidate lists
  -- also update Tidal "special" list
      liftIO $ writeIORef (listsR env) (updateTidalFolderAids am ls)

-- update the tags map with tags from this album
      tm <- readIORef (tagsR env)
      _ <- writeIORef (tagsR env) (updateTags a tm)

  -- update lists and folders
      newFolders <- readFolders env -- readDiscogsFolders
      -- reread Discogs lists info
      lm <- rereadLists env
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

envGetEnvr :: Env -> IO EnvR
envGetEnvr env = do
      am <- readIORef (albumsR env)
      lm <- readIORef (listsR env)
      lcs <- readIORef (locsR env)
      lns <- readIORef (listNamesR env)
      sn <- readIORef (sortNameR env)
      so <- readIORef (sortOrderR env)
      di <- readIORef (discogsR env)
      tm <- readIORef (tagsR env)
      pure $ EnvR am lm lcs lns sn so di tm

envGetTag :: Env -> Text -> IO (Vector Int)
envGetTag env t = do
  tm <- readIORef (tagsR env)
  case M.lookup t tm of
    Just v  -> pure v
    Nothing -> pure V.empty

envUpdateSort :: Env -> Maybe Text -> Maybe Text -> IO EnvR
envUpdateSort env msb mso = do
  am <- liftIO (readIORef (albumsR env))
  lns <- liftIO (readIORef (listNamesR env))
  lm <- liftIO (readIORef (listsR env))
  lcs <- liftIO (readIORef (locsR env))
  di <- liftIO (readIORef (discogsR env))
  tm <- liftIO (readIORef (tagsR env))
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
  pure $ EnvR am lm lcs lns sn so di tm

