{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Env
  ( refreshEnv,
    initEnv,
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
  )
import Provider
  ( readAlbums,
    readFolderAids,
    readFolders,
    readListAids,
    readLists,
    rereadLists,
  )
import Relude
import Types
  ( Album (..),
    Discogs (..),
    DiscogsInfo (..),
    Env (..),
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

initEnv :: IO Env
initEnv = getEnv Nothing Nothing

refreshEnv :: Env -> Text -> Text -> IO Env
refreshEnv env tok un = getEnv (Just env) (Just (Discogs $ DiscogsSession tok un))

-- myFoldr :: (a -> b -> b) -> b -> [a] -> b
-- myFoldr = _

-- initialize env: if not exest yet, get from file, otherwise from Discogs
getEnv :: Maybe Env -> Maybe Discogs -> IO Env
getEnv _ Nothing = envFromFiles
getEnv Nothing _ = envFromFiles
getEnv (Just env) (Just discogs') = do
  putTextLn $ "-----------------Updating from " <> show discogs'
  -- we still need the "old" lists map and album map
  oldAlbums <- readIORef $ albumsR env
  oldLists <- readIORef $ listsR env

  -- also save tidal albums
  let (_, tl) = fromMaybe (0, V.empty) $ M.lookup "Tidal" oldLists
      vta :: Vector Album
      vta = V.fromList $ mapMaybe (`M.lookup` oldAlbums) $ V.toList tl
      tidalAlbums = M.fromList $ (\a -> (albumID a, a)) <$> V.toList vta

  -- reread Discogs albums info, overwriting changes
  vda <- readAlbums discogs'
  let newAlbums :: Map Int Album
      newAlbums = M.fromList $ (\a -> (albumID a, a)) <$> V.toList vda
  let allAlbums = newAlbums <> tidalAlbums
  _ <- writeIORef (albumsR env) allAlbums

  -- reread Discogs folders info
  newFolders <- readFolders discogs' -- readDiscogsFolders
  -- reread Discogs lists info
  lm <- rereadLists discogs'
  -- reread folder album ids
  let fm :: Map Text (Int, Vector Int)
      fm = readFolderAids newFolders allAlbums
  let allLists = lm <> fm
  _ <- M.traverseWithKey (\n (i, vi) -> putTextLn $ show n <> "--" <> show i <> ": " <> show (length vi)) allLists

  _ <- writeIORef (listsR env) allLists
  _ <- writeIORef (listNamesR env) (V.fromList . M.keys $ allLists)
  _ <- writeIORef (discogsR env) discogs'
  _ <- writeIORef (sortNameR env) "Default"
  _ <- writeIORef (sortOrderR env) Asc
  pure env

fromListMap :: (Text, (Int, Vector Int)) -> [(Int, (Text, Int))]
fromListMap (ln, (_, aids)) = zipWith (\idx aid -> (aid, (ln, idx))) [1 ..] (V.toList aids)

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
            discogs' <- readIORef (discogsR env)
            aids <- readListAids discogs' lid
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
  -- let tidal = Tidal $ TidalFile "data/traw1.json"
  let tidal = Tidal $ TidalSession userId sessionId countryCode accessToken
  -- let dc = Discogs $ DiscogsSession discogsToken discogsUser
  let dc = Discogs $ DiscogsFile "data/"

  -- vda/vta :: Vector of Album
  vta <- readAlbums tidal
  vda <- readAlbums dc

  let albums' :: Map Int Album
      albums' =
        M.fromList $
          (\a -> (albumID a, a)) <$> V.toList (vda <> vta)

  -- define sort functions and map to names
  let sDef :: Map Int Album -> SortOrder -> Vector Int -> Vector Int
      sDef _ so l = case so of
        Asc -> l
        _ -> V.reverse l
  let sortAsi :: Map Int Album -> Vector Int -> [(Int, Maybe Album)]
      sortAsi am = map (\aid -> (aid, M.lookup aid am)) . V.toList
      compareAsc f (_, a) (_, b) = comparing f a b
      compareDesc f (_, a) (_, b) = comparing f b a
  let sTitle :: Map Int Album -> SortOrder -> Vector Int -> Vector Int
      sTitle am so aids = V.fromList (fst <$> sortBy (comp so) (sortAsi am aids))
        where
          comp o = case o of
            Asc -> compareAsc (fmap albumTitle)
            Desc -> compareDesc (fmap albumTitle)
  let sArtist :: Map Int Album -> SortOrder -> Vector Int -> Vector Int
      sArtist am so aids = V.fromList (fst <$> sortBy (comp so) (sortAsi am aids))
        where
          comp o = case o of
            Asc -> compareAsc (fmap albumArtist)
            Desc -> compareDesc (fmap albumArtist)
  let sAdded :: Map Int Album -> SortOrder -> Vector Int -> Vector Int
      sAdded am so aids = V.fromList (fst <$> sortBy (comp so) (sortAsi am aids))
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

      getSort' :: Map Int Album -> Text -> (SortOrder -> Vector Int -> Vector Int)
      getSort' am sn = fromMaybe sDef (M.lookup sn sfs) am

      sorts' :: Vector Text
      sorts' = V.fromList $ M.keys sfs
  -- read the map of Discogs lists (still empty album ids)
  lm <- readLists dc

  -- read the map of Discogs folders
  -- fm' :: Map Text Int
  fm' <- readFolders dc
  let fm :: Map Text (Int, Vector Int)
      fm = readFolderAids fm' albums'

  let lists' = lm <> fm
  let listNames' = V.fromList (M.keys lists')
  _ <- M.traverseWithKey (\n (i, vi) -> putTextLn $ show n <> "--" <> show i <> ": " <> show (length vi)) lists'
  let allLocs = M.fromList . concatMap fromListMap . filter (pLocList . fst) . M.toList $ lm

  -- store DiscogsSession after reading from cache files
  let dc' = Discogs $ DiscogsSession discogsToken discogsUser

  lnr <- newIORef listNames'
  lr <- newIORef lists'
  lo <- newIORef allLocs
  dr <- newIORef dc'
  ar <- newIORef albums'
  sr <- newIORef "Default"
  so <- newIORef Asc
  pure
    Env
      { discogsR = dr,
        albumsR = ar,
        listsR = lr,
        locsR = lo,
        listNamesR = lnr,
        sortNameR = sr,
        sortOrderR = so,
        sorts = sorts',
        url = "/",
        getList = getList',
        getSort = getSort'
      }
