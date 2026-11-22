{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- Phase 1 of docs/album-page-plan.md: the album strips of the album page, from what the
-- server already holds — other versions of an album, more by its artists, and similar
-- albums that have not been played for long. Pure; nothing is fetched.
module Related (
  AlbumRef (..),
  Related (..),
  noRelated,
  related,
) where

import Data.Aeson (ToJSON)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Relude
import Types (Album (..), AlbumKey (..), ArtistRef (..))

-- an album in a strip: enough to draw a card and to open the album
data AlbumRef = AlbumRef
  { rId :: AlbumKey
  , rTitle :: Text
  , rArtist :: Text
  , rCover :: Text
  , rYear :: Text
  , rFormat :: Text
  , rNote :: Text -- "last played 2023-04-11", or ""
  }
  deriving (Show, Generic)
instance ToJSON AlbumRef

data Related = Related
  { relVersions :: [AlbumRef] -- other collection releases of the same Discogs master
  , relByArtist :: [AlbumRef] -- other albums that share an artist, oldest first
  , relSimilar :: [AlbumRef] -- same genre, most shared styles first, then longest not played
  , relSimilarName :: Text -- the genre those were picked by, "" if none
  }
  deriving (Show, Generic)
instance ToJSON Related

noRelated :: Related
noRelated = Related [] [] [] ""

maxByArtist, maxSimilar :: Int
maxByArtist = 24
maxSimilar = 12

related :: Map AlbumKey Album -> Album -> Related
related am a =
  Related
    { relVersions = ref <$> sortOn albumReleased versions
    , -- if there are too many, those that share most artists; shown oldest first
      relByArtist = ref <$> sortOn albumReleased (take maxByArtist (sortOn (Down . sharedArtists) byArtist))
    , relSimilar = ref <$> take maxSimilar (sortOn (\b -> (Down (sharedStyles b), lastPlay b)) similar)
    , relSimilarName = fromMaybe "" genre
    }
 where
  -- a Tidal or Apple album that a Discogs release links to is the same album again
  linked f = S.fromList [i | b <- M.elems am, isDiscogs b, Just i <- [f b], i /= ""]
  linkedTidal = linked albumTidal
  linkedApple = linked albumAMusic
  shadowed b = case albumID b of
    T n -> show n `S.member` linkedTidal
    A n -> show n `S.member` linkedApple
    _ -> False
  others = [b | b <- M.elems am, albumID b /= albumID a, not (shadowed b), not (sameStream b)]
  -- the album's own stream, when the album is the Discogs release (or the other way round)
  sameStream b = case (albumID a, albumID b) of
    (D _, T n) -> albumTidal a == Just (show n)
    (D _, A n) -> albumAMusic a == Just (show n)
    (T n, D _) -> albumTidal b == Just (show n)
    (A n, D _) -> albumAMusic b == Just (show n)
    _ -> False

  masters b = S.fromList [k | k <- albumWikiKeys b, "dm:" `T.isPrefixOf` k]
  myMasters = masters a
  versions = [b | b <- others, not (S.null myMasters), not (S.disjoint myMasters (masters b))]
  versionIds = S.fromList (albumID <$> versions)

  -- artists by their store key and by name, so a Tidal album meets the Discogs releases
  artistIds b =
    S.fromList $
      [k | r <- albumArtists b, let k = arKey r, k /= ""]
        <> [n | r <- albumArtists b, let n = T.toCaseFold (arName r), n `notElem` ["", "various", "various artists"]]
  myArtists = artistIds a
  sharedArtists b = S.size (S.intersection myArtists (artistIds b))
  byArtist = [b | b <- others, albumID b `S.notMember` versionIds, sharedArtists b > 0]

  -- genre tags are "genre.<name>"; Discogs styles are the tags without a dot
  genre = find ("genre." `T.isPrefixOf`) (albumTags a)
  styles b = S.fromList [t | t <- albumTags b, not (T.any (== '.') t)]
  myStyles = styles a
  sharedStyles b = S.size (S.intersection myStyles (styles b))
  lastPlay b = fromMaybe "" (viaNonEmpty head (albumListenedDates b)) -- newest first
  similar =
    [ b
    | Just g <- [genre]
    , b <- others
    , g `elem` albumTags b
    , not (null (albumListenedDates b))
    , sharedArtists b == 0
    , albumID b `S.notMember` versionIds
    ]

  ref b =
    AlbumRef
      { rId = albumID b
      , rTitle = albumTitle b
      , rArtist = case filter (not . T.null) (arName <$> albumArtists b) of
          [] -> albumArtist b
          ns -> T.intercalate ", " ns
      , rCover = albumCover b
      , rYear = let y = T.take 4 (albumReleased b) in if y == "0" then "" else y -- Discogs: 0 = no year
      , rFormat = albumFormat b
      , rNote = case albumListenedDates b of
          (d : _) -> "last played " <> T.take 10 d
          [] -> ""
      }
  isDiscogs b = case albumID b of
    D _ -> True
    _ -> False
