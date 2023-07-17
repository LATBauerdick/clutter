-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Provider
  (
  -- AppM
    readListAids,
    readAlbum,
    readAlbums,
    readFolders,
    readLists,
  -- IO
    readDiscogsAlbums,
    readDiscogsLists,
    readDiscogsFolders,
    readTidalAlbums,
    readAMusicAlbums,
  -- Else
    readFolderAids,
    updateTidalFolderAids,
    updateAMusicFolderAids,
  )
where

-- import Data.Text.Encoding ( decodeUtf8 )
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified FromDiscogs as FD
  ( readDiscogsFolders,
    readDiscogsFoldersCache,
    readDiscogsListsCache,
    readDiscogsRelease,
    readDiscogsReleases,
    readDiscogsReleasesCache,
    readListAids,
    readLists,
  )
import qualified FromTidal as FT (readTidalReleases, readTidalReleasesCache)
import qualified FromAMusic as FA (readAMusicReleases)
import Relude
import Types
  ( Album (..),
    Discogs (..),
    Release (..),
    TagFolder (..),
    Tidal (..),
    TidalInfo (..),
    getTidal,
    AMusic (..),
    getAMusic,
    AppM,
    Env (..),
    envGetDiscogs,
    envGetListName,
  )
-- import Relude.Debug ( trace )
-- debug :: a -> Text -> a
-- debug a b = trace (toString b) a

dToAlbum :: Release -> Album
dToAlbum r =
  Album
    (daid r)
    (dtitle r)
    (T.intercalate ", " $ dartists r)
    (dreleased r)
    (dcover r)
    (dadded r)
    (dfolder r)
    (makeDiscogsURL (daid r))
    (dformat r)
    (dtidalid r)
    (damid r)
    (dlocation r)
    (dtags r)
    (drating r)
    (dplays r)
  where
    makeDiscogsURL a = T.pack $ "https://www.discogs.com/release/" ++ show a

readLists :: AppM (Map Text (Int, Vector Int))
readLists = do
  p <- envGetDiscogs
  case p of
    DiscogsFile fn -> error $ "Bug: Provider Discogs does not read lists from files " <> toText fn
    _ -> liftIO $ FD.readLists p

readDiscogsLists :: Discogs -> IO (Map Text (Int, Vector Int))
readDiscogsLists (DiscogsFile fn) = FD.readDiscogsListsCache fn
readDiscogsLists  di = FD.readLists di

readAlbum :: Int -> AppM (Maybe Album)
readAlbum aid = do
  p <- envGetDiscogs
  lns <- asks listNamesR >>= readIORef
  -- let getFolderName :: Int -> Maybe Text
  --     getFolderName fid = fmap fst . find (\(_, li) -> li == fid) $ M.toList lns
  d <- case p of
    DiscogsSession _ _ -> liftIO $ FD.readDiscogsRelease p lns aid
    _ -> pure Nothing
  let a = dToAlbum <$> d
  putTextLn $ "Retrieved Discogs Album " <> "\"" <> maybe "Nothing" albumTitle a <> "\""
  pure a

readAlbums :: Int -> AppM (Vector Album)
readAlbums nreleases = do
    lns <- asks listNamesR >>= readIORef
    di <- envGetDiscogs
    ds <- case di of
      DiscogsFile fn -> liftIO $ FD.readDiscogsReleasesCache fn lns
      _              -> liftIO $ FD.readDiscogsReleases di lns nreleases
    let as = dToAlbum <$> ds
    putTextLn $ "Total # Discogs Albums read: " <> show (length as)
    pure $ V.fromList as

readDiscogsAlbums :: Discogs -> Map Text Int -> IO (Vector Album)
readDiscogsAlbums di lns = do
    ds <- case di of
      DiscogsFile fn -> FD.readDiscogsReleasesCache fn lns
      _              -> FD.readDiscogsReleases di lns 0
    let as = dToAlbum <$> ds
    putTextLn $ "Total # Discogs Albums read: " <> show (length as)
    pure $ V.fromList as

readListAids :: Int -> AppM (Vector Int)
readListAids i = do
  di <- envGetDiscogs
  ln <- envGetListName i
  putTextLn $ "-----------------Getting List " <> show i <> " >>" <> fromMaybe "???" ln <> "<< from Discogs-----"
  case di of
        DiscogsFile _ -> pure V.empty -- maybe not ok
        _ -> liftIO $ FD.readListAids di i

readFolders :: AppM (Map Text Int)
readFolders = do
  di <- envGetDiscogs
  case di of
    DiscogsFile fn -> liftIO $ FD.readDiscogsFoldersCache fn
    _ -> liftIO $ FD.readDiscogsFolders di

readDiscogsFolders :: Discogs -> IO (Map Text Int)
readDiscogsFolders ( DiscogsFile fn ) = FD.readDiscogsFoldersCache fn
readDiscogsFolders di = FD.readDiscogsFolders di

-- populate the aids for folders from the folder+id in each Album
-- special treatment for Tidal, Discogs, and All folders
updateTidalFolderAids :: Map Int Album -> Map Text (Int, Vector Int) -> Map Text (Int, Vector Int)
updateTidalFolderAids am = M.insert "Tidal" (fromEnum TTidal, allTidal) where
    -- for Tidal folder, replace anything that's also on Discogs
    tidalToDiscogs :: Map Int Int
    tidalToDiscogs = M.fromList
                      . mapMaybe (\a
                          -> case readMaybe . toString =<< albumTidal a of
                                Just i  -> if i == albumID a then Nothing else Just (i , albumID a)
                                Nothing -> Nothing)
                      . M.elems
                      $ am
    allTidal  = V.map (\i -> fromMaybe i (i `M.lookup` tidalToDiscogs))
              . sAdded
              .  V.map fst
              . V.filter (\(_, f) -> f == fromEnum TTidal)
              . V.map (\a -> (albumID a, albumFolder a))
              .  V.fromList $ M.elems am
    sAdded :: Vector Int -> Vector Int
    sAdded aids = V.fromList (fst <$> sortBy (\(_, a) (_, b) -> comparing (fmap albumAdded) b a) asi)
      where
        asi :: [(Int, Maybe Album)]
        asi = map (\aid -> (aid, M.lookup aid am)) $ V.toList aids

-- populate the aids for folders from the folder+id in each Album
updateAMusicFolderAids :: Map Int Album -> Map Text (Int, Vector Int) -> Map Text (Int, Vector Int)
updateAMusicFolderAids am = M.insert "Apple Music" (fromEnum TAMusic, allAMusic) where
    -- for "AppleMusic" folder, replace anything that's also on Discogs
    aMusicToDiscogs :: Map Int Int
    aMusicToDiscogs = M.fromList
                      . mapMaybe (\a
                          -> case readMaybe . toString =<< albumAMusic a of
                                Just i  -> if i == albumID a then Nothing else Just (i , albumID a)
                                Nothing -> Nothing)
                      . M.elems
                      $ am
    allAMusic  = V.map (\i -> fromMaybe i (i `M.lookup` aMusicToDiscogs))
              . sAdded
              .  V.map fst
              . V.filter (\(_, f) -> f == fromEnum TAMusic)
              . V.map (\a -> (albumID a, albumFolder a))
              .  V.fromList $ M.elems am
    sAdded :: Vector Int -> Vector Int
    sAdded aids = V.fromList (fst <$> sortBy (\(_, a) (_, b) -> comparing (fmap albumAdded) b a) asi)
      where
        asi :: [(Int, Maybe Album)]
        asi = map (\aid -> (aid, M.lookup aid am)) $ V.toList aids


readFolderAids :: Map Text Int -> Map Int Album -> Map Text (Int, Vector Int)
readFolderAids fm am = fam
  where
    fam' = M.map getFolder fm
    fam = updateTidalFolderAids am
        . updateAMusicFolderAids am
        . M.insert "Discogs" (fromEnum TDiscogs, allDiscogs)
        . M.insert "All" (fromEnum TAll, allAlbums)
        $ fam'
    allAlbums = sAdded
              . V.map albumID
              . V.fromList $ M.elems am
    allDiscogs  = sAdded
                . V.map fst
                . V.filter (\(_, f) -> f /= fromEnum TTidal && f /= fromEnum TAMusic )
                . V.map (\a -> (albumID a, albumFolder a))
                . V.fromList $ M.elems am
    getFolder :: Int -> (Int, Vector Int)
    getFolder i = (i, filtFolder i)
    filtFolder :: Int -> Vector Int
    filtFolder fid  = sAdded
                    . V.map fst
                    . V.filter (\(_, f) -> f == fid)
                    . V.map (\a -> (albumID a, albumFolder a))
                    . V.fromList $ M.elems am
    sAdded :: Vector Int -> Vector Int
    sAdded aids = V.fromList (fst <$> sortBy (\(_, a) (_, b) -> comparing (fmap albumAdded) b a) asi)
      where
        asi :: [(Int, Maybe Album)]
        asi = map (\aid -> (aid, M.lookup aid am)) $ V.toList aids

-- items[].item.type
-- "SINGLE"
-- "ALBUM"
-- "EP"

-- link : http://www.tidal.com/album/aid
--             https://listen.tidal.com/album/
--             https://www.discogs.com/release/

-- items[].item.audioQuality
-- LOSSLESS
-- HI_RES
-- HIGH


readTidalAlbums :: Tidal -> IO (Vector Album)
readTidalAlbums p = do
    let
        ttoCoverURL r =
          T.concat
            [ T.pack "https://resources.tidal.com/images/",
              T.intercalate "/" $ T.splitOn "-" (dcover r),
              T.pack "/320x320.jpg"
            ]
        -- tgetAlbumURL :: Album -> Text
        -- tgetAlbumURL a = makeTidalURL (albumID a)
        makeTidalURL :: Int -> Text
        makeTidalURL tid =
          T.pack $ "https://listen.tidal.com/album/" ++ show tid

        toAlbum r =
          Album
            (daid r)
            (dtitle r)
            (T.intercalate ", " $ dartists r)
            (dreleased r)
            (ttoCoverURL r)
            (dadded r)
            (fromEnum TTidal)
            (makeTidalURL (daid r))
            "Tidal"
            (Just (show (daid r)))
            Nothing
            Nothing
            (dtags r)
            0
            0

    ds <- case getTidal p of
      TidalFile fn -> FT.readTidalReleasesCache fn
      _ -> FT.readTidalReleases (getTidal p)
    let as = toAlbum <$> ds
    putTextLn $ "Total # Tidal Albums: " <> show (length as)
    -- print $ drop (length as - 4) as
    pure $ V.fromList as

readAMusicAlbums :: AMusic -> IO (Vector Album)
readAMusicAlbums p = do
  let
      atoCoverURL r = T.replace "{h}" "320" $ T.replace "{w}" "320" (dcover r)
      makeAMusicURL cid = "https://beta.music.apple.com/library/albums/" <> cid
      toAlbum r =
        Album
          (daid r)
          (dtitle r)
          (T.intercalate ", " $ dartists r)
          (dreleased r)
          (atoCoverURL r)
          (dadded r)
          (fromEnum TAMusic)
          (makeAMusicURL $ fromMaybe "" (dlocation r))
          "AppleMusic"
          Nothing
          (damid r)
          (dlocation r)
          (dtags r)
          0
          0
  ds <- FA.readAMusicReleases (getAMusic p)
  let as = toAlbum <$> ds
  -- print as
  putTextLn $ "Total # Apple Music Albums: " <> show (length as)
  pure $ V.fromList as

