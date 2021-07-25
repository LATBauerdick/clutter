{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Provider
  ( readListAids,
    readAlbum,
    readAlbums,
    readLists,
    readFolders,
    readFolderAids,
    updateTidalFolderAids,
    rereadLists,
    atest,
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
    readDiscogsLists,
    readDiscogsListsCache,
    readDiscogsRelease,
    readDiscogsReleases,
    readDiscogsReleasesCache,
    readListAids,
    rereadLists,
  )
import qualified FromTidal as FT (readTidalReleases, readTidalReleasesCache)
import Relude
import Types
  ( Album (..),
    Discogs (..),
    DiscogsInfo (..),
    Release (..),
    TagFolder (..),
    Tidal (..),
    TidalInfo (..),
    getDiscogs,
    getTidal,
  )
-- import Relude.Debug ( trace )
debug :: a -> Text -> a
debug a b = trace (toString b) a

atest :: [Album]
atest =
  [ Album 161314 "Mezzanine" "Massive Attack" "2001" "161314.jpg" "2018-01-01T18:01:42-08:00" 1349997 "https://www.tidal.com/album/161314" "Vinyl" Nothing Nothing Nothing [] 0 0,
    Album 5253301 "Beethoven - Symphonie Nr. 3 »Eroica« & 4" "Herbert von Karajan" "1992" "5253301.jpg" "2017-09-17T20:57:52-07:00" 1351871 "https://www.tidal.com/album/5253301" "Vinyl" Nothing Nothing Nothing [] 0 0
  ]

class Provider p where
  readAlbums :: p -> IO (Vector Album)
  readAlbum :: p -> Int -> IO (Maybe Album)
  readLists :: p -> IO (Map Text (Int, Vector Int))

instance Provider Tidal where
  readLists _ = error "Bug: Provider Tidal has no lists"
  readAlbum _ _ = error "Not Implemented"
  readAlbums p = do
    let ttoCoverURL r =
          T.concat
            [ T.pack "https://resources.tidal.com/images/",
              T.intercalate "/" $ T.splitOn "-" (dcover r),
              T.pack "/320x320.jpg"
            ]
        tgetAlbumURL :: Album -> Text
        tgetAlbumURL a = makeTidalURL (albumID a)
        makeTidalURL :: Int -> Text
        makeTidalURL tid =
          T.pack $ "https://listen.tidal.com/album/" ++ show tid

    ds <- case getTidal p of
      TidalFile fn -> FT.readTidalReleasesCache fn
      _ -> FT.readTidalReleases (getTidal p)
    let as = toAlbum <$> ds
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
            ["tidal"]
            0
            0

    putTextLn $ "Total # Tidal Albums: " <> show (length as)
    -- print $ drop (length as - 4) as

    pure $ V.fromList as

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
    format
    (dtidalid r)
    (damid r)
    (dlocation r)
    (dtags r)
    (drating r)
    (dplays r)
  where
    makeDiscogsURL a = T.pack $ "https://www.discogs.com/release/" ++ show a
-- LATB Hack, needs to be fixed!!!! XXXXXXXX
    format = case dfolder r of
                3292597 -> "Streaming"
                3141493 -> "File"
                _       -> T.intercalate ", " $ dformat r




instance Provider Discogs where
  readLists p = case getDiscogs p of
    DiscogsFile fn -> FD.readDiscogsListsCache fn
    _ -> FD.readDiscogsLists (getDiscogs p)
  readAlbum p aid = do
    d <- case getDiscogs p of
      DiscogsSession _ _ -> FD.readDiscogsRelease (getDiscogs p) aid
      _ -> pure Nothing
    let a = dToAlbum <$> d
    putTextLn $ "Retrieved Discogs Album " <> show (albumTitle <$> a)
    pure a

  readAlbums p = do
    ds <- case getDiscogs p of
      DiscogsFile fn -> FD.readDiscogsReleasesCache fn
      _ -> FD.readDiscogsReleases (getDiscogs p)

    let as = dToAlbum <$> ds

    putTextLn $ "Total # Discogs Albums: " <> show (length as)
    -- print $ drop ( length as - 4 ) as

    pure $ V.fromList as

readListAids :: Discogs -> Int -> IO (Vector Int)
readListAids p i = case getDiscogs p of
  DiscogsFile _ -> pure V.empty -- maybe not ok
  _ -> FD.readListAids (getDiscogs p) i

readFolders :: Discogs -> IO (Map Text Int)
readFolders p = case getDiscogs p of
  DiscogsFile fn -> FD.readDiscogsFoldersCache fn
  _ -> FD.readDiscogsFolders (getDiscogs p)

-- populate the aids for folders from the folder+id in each Album
-- special treatment for Tidal, Discogs, and All folders
updateTidalFolderAids :: Map Int Album -> Map Text (Int, Vector Int) -> Map Text (Int, Vector Int)
updateTidalFolderAids am = M.insert "Tidal" (fromEnum TTidal, allTidal) where
    -- for Tidal folder, replace anything that's also on Discogs
    xxx :: [(Int, Int)]
    xxx = mapMaybe (\a -> case readMaybe . toString =<< albumTidal a of
                            Just i -> if i == albumID a then Nothing else Just (i , albumID a)
                            Nothing -> Nothing
                   )
        $ M.elems am
    tidalToDiscogs = M.fromList xxx `debug` show xxx
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


readFolderAids :: Map Text Int -> Map Int Album -> Map Text (Int, Vector Int)
readFolderAids fm am = fam
  where
    fam' = M.map getFolder fm
    fam = updateTidalFolderAids am
        . M.insert "Discogs" (fromEnum TDiscogs, allDiscogs)
        . M.insert "All" (fromEnum TAll, allAlbums)
        $ fam'
    allAlbums = sAdded
              . V.map albumID
              . V.fromList $ M.elems am
    allDiscogs  = sAdded
                . V.map fst
                . V.filter (\(_, f) -> f /= fromEnum TTidal)
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

rereadLists :: Discogs -> IO (Map Text (Int, Vector Int))
rereadLists p = case getDiscogs p of
  DiscogsFile fn -> error $ "Bug: Provider Discogs does not reread lists from files " <> toText fn
  _ -> FD.rereadLists (getDiscogs p)

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
