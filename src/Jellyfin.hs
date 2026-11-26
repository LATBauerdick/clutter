{-# LANGUAGE LambdaCase #-}

-- Albums on the file server, as Jellyfin knows them (docs/jellyfin-plan.md).
--
-- scripts/update-jellyfin-albums.py fills ~/Sync/clutter/jellyfin-albums.json
-- (CLUTTER_JELLYFIN_ALBUMS overrides) from Jellyfin's API; the server only reads that
-- snapshot. Every album in it becomes a J<id> album with the tags provider.jellyfin and
-- storage.<name> (the storage location: the Jellyfin library, or a folder of it, as
-- ~/Sync/clutter/storage-map.json says). A missing or unreadable store means no Jellyfin
-- albums, no error.
--
-- The address a browser reaches Jellyfin at (covers, "Open in Jellyfin") is the store's
-- "web", or CLUTTER_JELLYFIN; empty means there are no such links.
module Jellyfin (
  JellyfinStore (..),
  readJellyfinStore,
  jellyfinAlbums,
  jellyfinSecs,
  jellyfinPaths,
  jellyfinApi,
) where

import Data.Aeson (FromJSON (..), Value, eitherDecode, withObject, (.!=), (.:?))
import Data.Aeson.Types (parseMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Relude
import Data.Time (UTCTime)
import System.Directory (doesFileExist, getModificationTime)
import System.IO.Unsafe (unsafePerformIO)
import System.FilePath ((</>))
import System.IO (hPutStrLn)
import Types (Album (..), AlbumKey (..), ArtistRef (..), TagFolder (..))

data JAlbum = JAlbum
  { jaTitle :: Text
  , jaArtist :: Text
  , jaArtists :: [Text]
  , jaYear :: Int
  , jaAdded :: Text
  , jaSecs :: Int
  , jaStorage :: Text
  , jaImage :: Bool
  , jaPath :: Text -- the album's folder as Jellyfin sees it, "" if the store has none
  }

instance FromJSON JAlbum where
  parseJSON = withObject "Jellyfin album" $ \o ->
    JAlbum
      <$> o .:? "title" .!= ""
      <*> o .:? "artist" .!= ""
      <*> o .:? "artists" .!= []
      <*> o .:? "year" .!= 0
      <*> o .:? "added" .!= ""
      <*> o .:? "secs" .!= 0
      <*> o .:? "storage" .!= ""
      <*> o .:? "image" .!= False
      <*> o .:? "path" .!= ""

data JellyfinStore = JellyfinStore
  { jsWeb :: Text -- Jellyfin as a browser reaches it, no trailing slash; "" if not known
  , jsAlbums :: Map Text JAlbum -- by Jellyfin item id, 32 hex digits, lower case
  }

data StoreFile = StoreFile Text (Map Text JAlbum)
instance FromJSON StoreFile where
  parseJSON = withObject "Jellyfin album store" $ \o ->
    StoreFile <$> o .:? "web" .!= "" <*> o .:? "albums" .!= M.empty

storePath :: IO FilePath
storePath =
  lookupEnv "CLUTTER_JELLYFIN_ALBUMS" >>= \case
    Just f | not (null f) -> pure f
    _ -> do
      home <- fromMaybe "." <$> lookupEnv "HOME"
      pure $ home </> "Sync" </> "clutter" </> "jellyfin-albums.json"

readJellyfinStore :: IO JellyfinStore
readJellyfinStore = do
  fn <- storePath
  webEnv <- maybe "" toText <$> lookupEnv "CLUTTER_JELLYFIN"
  let web w = T.dropWhileEnd (== '/') (if T.null webEnv then w else webEnv)
  ok <- doesFileExist fn
  if not ok
    then hPutStrLn stderr ("no Jellyfin albums at " <> fn <> " -- run scripts/update-jellyfin-albums.py") $> JellyfinStore (web "") M.empty
    else
      eitherDecode <$> readFileLBS fn >>= \case
        Left err -> hPutStrLn stderr ("Jellyfin albums " <> fn <> ": " <> err) $> JellyfinStore (web "") M.empty
        Right (StoreFile w as) -> do
          let as' = M.mapKeys T.toLower as
          hPutStrLn stderr $ "read " <> show (M.size as') <> " Jellyfin albums from " <> fn
          pure (JellyfinStore (web w) as')

-- playing times in seconds, where Jellyfin has one
jellyfinSecs :: JellyfinStore -> Map Text Int
jellyfinSecs = M.filter (> 0) . M.map jaSecs . jsAlbums

jellyfinAlbums :: JellyfinStore -> [Album]
jellyfinAlbums (JellyfinStore web as) = uncurry toAlbum <$> M.toList as
 where
  have = not (T.null web)
  toAlbum i a =
    Album
      { albumID = J i
      , albumInst = 0
      , albumTitle = jaTitle a
      , albumArtist = jaArtist a
      , albumReleased = if jaYear a > 0 then show (jaYear a) else ""
      , albumCover = if have && jaImage a then web <> "/Items/" <> i <> "/Images/Primary?fillWidth=320&fillHeight=320&quality=90" else ""
      , albumAdded = jaAdded a
      , albumFolder = fromEnum TJellyfin
      , albumURL = if have then web <> "/web/#/details?id=" <> i else ""
      , albumFormat = "Jellyfin"
      , albumTidal = Nothing
      , albumAMusic = Nothing
      , albumQobuz = Nothing
      , albumJellyfin = Just i
      , albumLocIdx = Nothing
      , albumLocation = Nothing
      , albumTags = "provider.jellyfin" : ["storage." <> jaStorage a | not (T.null (jaStorage a))]
      , albumRating = 0
      , albumPlays = 0
      , albumListenedDates = []
      , albumFavoriteUntil = Nothing
      , albumWikiKeys = []
      , albumWiki = Nothing
      , albumAllMusic = Nothing
      , albumArtists =
          [ ArtistRef{arName = n, arKey = "", arWiki = Nothing, arAllMusic = Nothing}
          | n <- if null (jaArtists a) then [jaArtist a | not (T.null (jaArtist a))] else jaArtists a
          ]
      , albumFiles = []
      }

-- Jellyfin as the server reaches its API: the store's "api" (what the update script used),
-- no trailing slash; "" if there is none
jellyfinApi :: IO Text
jellyfinApi = do
  fn <- storePath
  ok <- doesFileExist fn
  if not ok
    then pure ""
    else
      readFileLBS fn <&> \bs -> case eitherDecode bs :: Either String Value of
        Right v | Just a <- parseMaybe (withObject "store" (.:? "api")) v -> T.dropWhileEnd (== '/') (fromMaybe "" a)
        _ -> ""

-- The folder of every file album as Jellyfin sees it (item id -> "/media/music/…"), for
-- finding the same folder on a player that has the share (App: playing files on the Node).
-- Read from the store when first asked for and again when the file has changed.
jellyfinPaths :: IO (Map Text Text)
jellyfinPaths = do
  fn <- storePath
  ok <- doesFileExist fn
  if not ok
    then pure M.empty
    else do
      mt <- getModificationTime fn
      readIORef pathsCache >>= \case
        Just (t, ps) | t == mt -> pure ps
        _ ->
          eitherDecode <$> readFileLBS fn >>= \case
            Left _ -> pure M.empty
            Right (StoreFile _ as) -> do
              let ps = M.filter (not . T.null) (M.map jaPath (M.mapKeys T.toLower as))
              writeIORef pathsCache (Just (mt, ps))
              pure ps

{-# NOINLINE pathsCache #-}
pathsCache :: IORef (Maybe (UTCTime, Map Text Text))
pathsCache = unsafePerformIO (newIORef Nothing)
