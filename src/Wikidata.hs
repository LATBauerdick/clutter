{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- Phase 3 of docs/wikidata-plan.md: Wikipedia and AllMusic links for albums and artists,
-- from ~/Sync/clutter/wikidata.json (CLUTTER_WIKIDATA overrides), which
-- scripts/update-wikidata.py keeps. A missing or unreadable store means no links, no error.
module Wikidata (
  WikiStore,
  emptyWikiStore,
  readWikiStore,
  attachWiki,
  displayName,
) where

import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.!=), (.:?))
import qualified Data.Char as Ch
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Relude
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO (hPutStrLn)
import Types (Album (..), ArtistRef (..))

data WEntry = WEntry
  { weEnwiki :: Maybe Text
  , weDewiki :: Maybe Text
  , weAllMusic :: Maybe Text
  }
instance FromJSON WEntry where
  parseJSON = withObject "wikidata entry" $ \o ->
    WEntry <$> o .:? "enwiki" <*> o .:? "dewiki" <*> o .:? "allmusic"

data WikiStore = WikiStore
  { wsAlbums :: Map Text WEntry -- "dm:…", "t:…", "a:…"
  , wsArtists :: Map Text WEntry -- "da:…", "ta:…"
  }
instance FromJSON WikiStore where
  parseJSON = withObject "wikidata store" $ \o ->
    WikiStore <$> o .:? "albums" .!= M.empty <*> o .:? "artists" .!= M.empty

emptyWikiStore :: WikiStore
emptyWikiStore = WikiStore M.empty M.empty

wikiStorePath :: IO FilePath
wikiStorePath =
  lookupEnv "CLUTTER_WIKIDATA" >>= \case
    Just f | not (null f) -> pure f
    _ -> do
      home <- fromMaybe "." <$> lookupEnv "HOME"
      pure $ home </> "Sync" </> "clutter" </> "wikidata.json"

readWikiStore :: IO WikiStore
readWikiStore = do
  fn <- wikiStorePath
  ok <- doesFileExist fn
  if not ok
    then hPutStrLn stderr ("no Wikidata store at " <> fn <> " -- no Wikipedia/AllMusic links") $> emptyWikiStore
    else
      eitherDecode <$> readFileLBS fn >>= \case
        Left err -> hPutStrLn stderr ("Wikidata store " <> fn <> ": " <> err) $> emptyWikiStore
        Right ws -> do
          hPutStrLn stderr $
            "read Wikidata store " <> fn <> ": " <> show (M.size (wsAlbums ws)) <> " album keys, "
              <> show (M.size (wsArtists ws)) <> " artist keys"
          pure ws

-- a Discogs artist name without its disambiguation suffix: "John Williams (4)" -> "John Williams"
displayName :: Text -> Text
displayName n = case T.breakOnEnd " (" n of
  (pre, rest)
    | not (T.null pre)
    , Just d <- T.stripSuffix ")" rest
    , not (T.null d)
    , T.all Ch.isDigit d ->
        T.dropEnd 2 pre
  _ -> n

-- Wikipedia: English, else German (decision 1)
wikiOf :: WEntry -> Maybe Text
wikiOf e = weEnwiki e <|> weDewiki e

hasLink :: WEntry -> Bool
hasLink e = isJust (wikiOf e) || isJust (weAllMusic e)

-- fill albumWiki, albumAllMusic and the artists' links from the store; idempotent
attachWiki :: WikiStore -> Map k Album -> Map k Album
attachWiki ws am = M.map attach am
 where
  -- linked Discogs artists by case-folded display name, for artists without a link of their
  -- own (mostly Tidal artist ids, which Wikidata knows less often); only unambiguous names
  byName :: Map Text Text
  byName =
    M.mapMaybe (\ks -> if S.size ks == 1 then S.lookupMin ks else Nothing) $
      M.fromListWith
        S.union
        [ (T.toCaseFold (arName r), S.singleton (arKey r))
        | a <- M.elems am
        , r <- albumArtists a
        , "da:" `T.isPrefixOf` arKey r
        , maybe False hasLink (M.lookup (arKey r) (wsArtists ws))
        ]
  attach a =
    let es = mapMaybe (`M.lookup` wsAlbums ws) (albumWikiKeys a)
     in a
          { albumWiki = asum (wikiOf <$> es)
          , albumAllMusic = asum (fmap ("https://www.allmusic.com/album/" <>) . weAllMusic <$> es)
          , albumArtists = artist <$> albumArtists a
          }
  artist r =
    let own = mfilter hasLink (M.lookup (arKey r) (wsArtists ws))
        named = M.lookup (T.toCaseFold (arName r)) byName >>= (`M.lookup` wsArtists ws)
     in case own <|> named of
          Just e -> r{arWiki = wikiOf e, arAllMusic = ("https://www.allmusic.com/artist/" <>) <$> weAllMusic e}
          Nothing -> r{arWiki = Nothing, arAllMusic = Nothing}
