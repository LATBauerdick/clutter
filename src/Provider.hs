{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Provider (
  -- AppM
  readDiscogsListAids,
  readAlbum,
  readAlbums,
  readFolders,
  -- readLists,
  -- IO
  readDiscogsAlbums,
  readDiscogsLists,
  readDiscogsFolders,
  readTidalAlbums,
  readTidalDropped,
  readAMusicAlbums,
  -- Else
  readFolderAids,
  updateTidalFolderAids,
  updateAMusicFolderAids,
  linkTidalTags,
  standIns,
  memberList,
  serverList,
  allList,
)
where

-- import Data.Text.Encoding ( decodeUtf8 )
import qualified Data.Char as Ch
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified FromAMusic as FA (readAMusicReleases)
import qualified FromDiscogs as FD (
  readDiscogsFolders,
  readDiscogsFoldersCache,
  readDiscogsListAids,
  readDiscogsLists,
  readDiscogsRelease,
  readDiscogsReleases,
  readDiscogsReleasesCache,
 )
import qualified FromTidal as FT (readTidalDropped, readTidalReleases, readTidalReleasesCache, readTidalStore, tidalStorePath)
import System.Directory (doesFileExist)
import qualified Wikidata as W (displayName)
import Relude
import Types (
  AMusic (..),
  Album (..),
  ArtistRef (..),
  AlbumKey (..),
  AppM,
  Discogs (..),
  Env (..),
  Release (..),
  TagFolder (..),
  Tidal (..),
  TidalInfo (..),
  envGetDiscogs,
  envGetListName,
  getAMusic,
  getTidal,
 )

-- import Relude.Debug ( trace )
-- debug :: a -> Text -> a
-- debug a b = trace (toString b) a

dToAlbum :: Release -> Album
dToAlbum r =
  Album
    (D (daid r))
    (dinst r)
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
    (dqobuzid r)
    (djellyfinid r)
    (dlocIdx r)
    (dlocation r)
    (dtags r)
    (drating r)
    (dplays r)
    [] -- albumListenedDates, from the play log (Listened.attachPlays)
    Nothing -- albumFavoriteUntil
    (wikiKeys r) -- albumWikiKeys
    Nothing -- albumWiki, from wikidata.json (Wikidata.attachWiki)
    Nothing -- albumAllMusic
    (artistRefs r) -- albumArtists
    (maybe [] (one . T.toLower) (mfilter (not . T.null) (djellyfinid r))) -- albumFiles
 where

  makeDiscogsURL a = T.pack $ "https://www.discogs.com/release/" ++ show a

-- Wikidata store keys of a release, best first (docs/wikidata-plan.md): Discogs master, Tidal, Apple
wikiKeys :: Release -> [Text]
wikiKeys r =
  catMaybes
    [ ("dm:" <>) . show <$> dmaster r
    , ("t:" <>) <$> mfilter (not . T.null) (dtidalid r)
    , ("a:" <>) <$> mfilter (T.all Ch.isDigit) (mfilter (not . T.null) (damid r))
    ]

-- the release's artists, links still empty
artistRefs :: Release -> [ArtistRef]
artistRefs r = zipWith ref (dartists r) (dartistids r <> repeat "")
 where
  ref n k = ArtistRef{arName = W.displayName n, arKey = k, arWiki = Nothing, arAllMusic = Nothing}

-- Discogs lists hold Discogs releases
readDiscogsLists :: Discogs -> IO (Map Text (Int, Vector AlbumKey))
readDiscogsLists = fmap (M.map (second (V.map D))) . FD.readDiscogsLists

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
    _ -> liftIO $ FD.readDiscogsReleases di lns nreleases
  let as = dToAlbum <$> ds
  putTextLn $ "Total # Discogs Albums read: " <> show (length as)
  pure $ V.fromList as

readDiscogsAlbums :: Discogs -> Map Text Int -> IO (Vector Album)
readDiscogsAlbums di lns = do
  ds <- case di of
    DiscogsFile fn -> FD.readDiscogsReleasesCache fn lns
    _ -> FD.readDiscogsReleases di lns 0
  let as = dToAlbum <$> ds
  putTextLn $ "Total # Discogs Albums read: " <> show (length as)
  pure $ V.fromList as

readDiscogsListAids :: Int -> AppM (Vector AlbumKey)
readDiscogsListAids i = do
  di <- envGetDiscogs
  ln <- envGetListName i
  putTextLn $ "-----------------Getting List " <> show i <> " >>" <> fromMaybe "???" ln <> "<< from Discogs-----"
  liftIO $ V.map D <$> FD.readDiscogsListAids di i

readFolders :: AppM (Map Text Int)
readFolders = do
  di <- envGetDiscogs
  case di of
    DiscogsFile fn -> liftIO $ FD.readDiscogsFoldersCache fn
    _ -> liftIO $ FD.readDiscogsFolders di

readDiscogsFolders :: Discogs -> IO (Map Text Int)
readDiscogsFolders (DiscogsFile fn) = FD.readDiscogsFoldersCache fn
readDiscogsFolders di = FD.readDiscogsFolders di

-- the Tidal list: every Discogs release with a Tidal location (T<id>), and every Tidal album
-- clutter holds (ever a favourite, or played) that no Discogs release stands for
updateTidalFolderAids :: Map AlbumKey Album -> Map Text (Int, Vector AlbumKey) -> Map Text (Int, Vector AlbumKey)
updateTidalFolderAids am = M.insert "Tidal" (fromEnum TTidal, allTidal)
 where
  linkedD = [k | (k@(D _), a) <- M.toList am, isJust (tidalId a)]
  linkedT = standIns am
  tidalOnly = [k | k@(T _) <- M.keys am, not (k `S.member` linkedT)]
  allTidal = sAdded (V.fromList (linkedD <> tidalOnly))
  sAdded :: Vector AlbumKey -> Vector AlbumKey
  sAdded aids = V.fromList (fst <$> sortBy (\(_, a) (_, b) -> comparing (fmap albumAdded) b a) asi)
   where
    asi :: [(AlbumKey, Maybe Album)]
    asi = map (\aid -> (aid, M.lookup aid am)) $ V.toList aids

tidalId :: Album -> Maybe Int
tidalId a = readMaybe . toString =<< albumTidal a

-- The stream albums that another album stands for: a Tidal album named in the Location field
-- of a Discogs release, and an Apple Music album named by a Discogs release or a Tidal album.
-- A list shows the album that stands for them, never both — whatever the focus: the plays
-- and so the played.* tags are on the standing album, so a filter must not bring its twin back.
standIns :: Map AlbumKey Album -> Set AlbumKey
standIns am = S.fromList (concatMap twins (M.elems am))
 where
  twins a =
    [T t | D _ <- [albumID a], Just t <- [tidalId a]]
      <> [A i | Just i <- [readMaybe . toString =<< albumAMusic a], albumID a /= A i]
      <> [J j | notFile (albumID a), j <- albumFiles a]
  notFile = \case J _ -> False; _ -> True

-- the albums with a tag, newest first, without the stand-ins: the lists "Collection"
-- (collection.in) and "Inbox" (collection.inbox), read off the tags when asked for, so
-- they follow a play or a keep mark at once
memberList :: Text -> Map AlbumKey Album -> Vector AlbumKey
memberList tag am = V.fromList [albumID a | a <- sortOn (Down . albumAdded) (M.elems am), tag `elem` albumTags a, not (albumID a `S.member` hidden)]
 where
  hidden = standIns am

-- "All": every album, newest first, without the stand-ins; read off the album map when
-- asked for, since a link in the collection log changes who stands in for whom
allList :: Map AlbumKey Album -> Vector AlbumKey
allList am = V.fromList [albumID a | a <- sortOn (Down . albumAdded) (M.elems am), not (albumID a `S.member` hidden)]
 where
  hidden = standIns am

-- the albums on the file server, newest first: every file album, or those of one storage
-- location (tag storage.<name>). A file album that is a version of another album is shown
-- as that album, which carries its storage tags (Collection.attachLinks). Read off the tags
-- when asked for, like memberList.
serverList :: Maybe Text -> Map AlbumKey Album -> Vector AlbumKey
serverList mstorage am = V.fromList [albumID a | a <- sortOn (Down . albumAdded) (M.elems am), wanted a, not (albumID a `S.member` hidden)]
 where
  hidden = standIns am
  wanted a = case mstorage of
    Just s -> ("storage." <> s) `elem` albumTags a
    Nothing -> any ("storage." `T.isPrefixOf`) (albumTags a)

-- a Discogs release whose Tidal album is in the Tidal collection is tagged tidal.collection
-- too, so #tidal.collection filters the whole Tidal list; likewise tidal.gone, when its
-- Tidal album is off Tidal
linkTidalTags :: Map AlbumKey Album -> Map AlbumKey Album
linkTidalTags am = M.map link am
 where
  linked = ["tidal.collection", "tidal.gone"]
  having tag = S.fromList [k | (k@(T _), a) <- M.toList am, tag `elem` albumTags a]
  sets = [(tag, having tag) | tag <- linked]
  link a = case albumID a of
    D _ ->
      let others = filter (`notElem` linked) (albumTags a)
       in a{albumTags = others <> [tag | (tag, ks) <- sets, Just t <- [tidalId a], T t `S.member` ks]}
    _ -> a

-- the Tidal albums that were removed from clutter by hand (the album store's "dropped")
readTidalDropped :: IO [AlbumKey]
readTidalDropped = do
  store <- FT.tidalStorePath
  haveStore <- doesFileExist store
  if haveStore then map T <$> FT.readTidalDropped store else pure []

-- populate the aids for folders from the folder+id in each Album
updateAMusicFolderAids :: Map AlbumKey Album -> Map Text (Int, Vector AlbumKey) -> Map Text (Int, Vector AlbumKey)
updateAMusicFolderAids am = M.insert "Apple Music" (fromEnum TAMusic, allAMusic)
 where
  -- for "AppleMusic" folder, replace anything that's also on Discogs
  aMusicToDiscogs :: Map AlbumKey AlbumKey
  aMusicToDiscogs =
    M.fromList
      . mapMaybe
        ( \a ->
            case readMaybe . toString =<< albumAMusic a of
              Just i -> if A i == albumID a then Nothing else Just (A i, albumID a)
              Nothing -> Nothing
        )
      . M.elems
      $ am
  allAMusic =
    V.map (\i -> fromMaybe i (i `M.lookup` aMusicToDiscogs))
      . sAdded
      . V.map fst
      . V.filter (\(_, f) -> f == fromEnum TAMusic)
      . V.map (\a -> (albumID a, albumFolder a))
      . V.fromList
      $ M.elems am
  sAdded :: Vector AlbumKey -> Vector AlbumKey
  sAdded aids = V.fromList (fst <$> sortBy (\(_, a) (_, b) -> comparing (fmap albumAdded) b a) asi)
   where
    asi :: [(AlbumKey, Maybe Album)]
    asi = map (\aid -> (aid, M.lookup aid am)) $ V.toList aids

readFolderAids :: Map Text Int -> Map AlbumKey Album -> Map Text (Int, Vector AlbumKey)
readFolderAids fm am = fam
 where
  fam' = M.map getFolder fm
  fam =
    updateTidalFolderAids am
      . updateAMusicFolderAids am
      . M.insert "Discogs" (fromEnum TDiscogs, allDiscogs)
      . M.insert "All" (fromEnum TAll, allAlbums)
      $ fam'
  allAlbums =
    sAdded
      . V.filter (not . (`S.member` standIns am))
      . V.map albumID
      . V.fromList
      $ M.elems am
  allDiscogs =
    sAdded
      . V.map fst
      . V.filter (\(_, f) -> f /= fromEnum TTidal && f /= fromEnum TAMusic && f /= fromEnum TJellyfin)
      . V.map (\a -> (albumID a, albumFolder a))
      . V.fromList
      $ M.elems am
  getFolder :: Int -> (Int, Vector AlbumKey)
  getFolder i = (i, filtFolder i)
  filtFolder :: Int -> Vector AlbumKey
  filtFolder fid =
    sAdded
      . V.map fst
      . V.filter (\(_, f) -> f == fid)
      . V.map (\a -> (albumID a, albumFolder a))
      . V.fromList
      $ M.elems am
  sAdded :: Vector AlbumKey -> Vector AlbumKey
  sAdded aids = V.fromList (fst <$> sortBy (\(_, a) (_, b) -> comparing (fmap albumAdded) b a) asi)
   where
    asi :: [(AlbumKey, Maybe Album)]
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
        [ T.pack "https://resources.tidal.com/images/"
        , T.intercalate "/" $ T.splitOn "-" (dcover r)
        , T.pack "/320x320.jpg"
        ]
    -- tgetAlbumURL :: Album -> Text
    -- tgetAlbumURL a = makeTidalURL (albumID a)
    makeTidalURL :: Int -> Text
    makeTidalURL tid =
      -- T.pack $ "https://listen.tidal.com/album/" ++ show tid
      T.pack $ "tidal://album/" ++ show tid

    toAlbum r =
      Album
        (T (daid r))
        (dinst r)
        (dtitle r)
        (T.intercalate ", " $ dartists r)
        (dreleased r)
        (ttoCoverURL r)
        (dadded r)
        (fromEnum TTidal)
        (makeTidalURL (daid r))
        "Tidal"
        (Just (show (daid r)))
        (damid r) -- Apple Music id, from the Tidal album store
        Nothing
        Nothing
        Nothing
        Nothing
        (dtags r)
        0
        0
        [] -- albumListenedDates, from the play log
        (dfavuntil r) -- albumFavoriteUntil
        (wikiKeys r) -- albumWikiKeys
        Nothing -- albumWiki, from wikidata.json (Wikidata.attachWiki)
        Nothing -- albumAllMusic
        (artistRefs r) -- albumArtists
        [] -- albumFiles, from the collection log
  -- clutter's Tidal album store holds every album that was ever a favourite or was played;
  -- without it, the favourites dump as before
  store <- FT.tidalStorePath
  haveStore <- doesFileExist store
  ds <- case getTidal p of
    TidalFile _ | haveStore -> FT.readTidalStore store
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
        (A (daid r))
        (dinst r)
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
        Nothing
        Nothing
        Nothing
        (dlocation r)
        (dtags r)
        0
        0
        [] -- albumListenedDates, from the play log
        (dfavuntil r) -- albumFavoriteUntil
        (wikiKeys r) -- albumWikiKeys
        Nothing -- albumWiki, from wikidata.json (Wikidata.attachWiki)
        Nothing -- albumAllMusic
        (artistRefs r) -- albumArtists
        [] -- albumFiles, from the collection log
  ds <- FA.readAMusicReleases (getAMusic p)
  let as = toAlbum <$> ds
  -- print as
  putTextLn $ "Total # Apple Music Albums: " <> show (length as)
  pure $ V.fromList as
