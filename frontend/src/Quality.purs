module Quality (
  qualityRank
, bestVersion
) where

-- docs/album-layer-plan.md › Slice 9: the album page starts on the playable version of the
-- best quality. Hi-Res (24-bit or above 48 kHz, DSD, Tidal's "Hi-Res lossless") › Lossless
-- (16-bit up to 48 kHz, Tidal's "Lossless") › lossy › unknown; a tie goes to Tidal (it starts
-- faster on the Node), then to the version the page was opened as.

import Prelude

import Data.Array (elem, filter, foldl, head, mapMaybe, null)
import Data.Int (fromString) as I
import Data.Number (fromString) as N
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), contains, split, stripSuffix, take, toUpper, trim)

import Types (Album, AlbumXJ)

-- one quality text as the backend gives it: Tidal's ("Hi-Res lossless", "Lossless",
-- "Lossless · Dolby Atmos") or the files' ("FLAC 24/96", "ALAC 16/44.1", "DSD64",
-- "MP3 320", several joined with " · "). Several: the lowest counts. 0: unknown.
qualityRank :: String -> Int
qualityRank q =
  let unEllipsis p = fromMaybe p (stripSuffix (Pattern "…") p)
      parts = filter (\p -> p /= "" && p /= "Dolby Atmos") (map (trim <<< unEllipsis <<< trim) (split (Pattern " · ") q))
  in if null parts then 0 else foldl min 3 (map partRank parts)

partRank :: String -> Int
partRank p
  | contains (Pattern "Hi-Res") p = 3
  | contains (Pattern "Lossless") p = 2
  | take 3 (toUpper p) == "DSD" = 3
  | elem (toUpper (fromMaybe "" (head (wordsOf p)))) [ "MP3", "AAC", "OGG", "OPUS", "WMAV2" ] = 1
  | otherwise = resRank (wordsOf p)

wordsOf :: String -> Array String
wordsOf = split (Pattern " ")

-- "FLAC 24/96", "ALAC 16/44.1", "PCM 44.1": Hi-Res above 16 bit or 48 kHz; no resolution
-- given: Lossless
resRank :: Array String -> Int
resRank ws = case ws of
  [ _, res ] -> case split (Pattern "/") res of
    [ b, r ] -> if fromMaybe 0 (I.fromString b) > 16 || khz r > 48.0 then 3 else 2
    [ r ] -> if khz r > 48.0 then 3 else 2
    _ -> 2
  _ -> 2
  where
  khz r = fromMaybe 0.0 (N.fromString r)

type Cand = { key :: String, rank :: Int, tidal :: Boolean, own :: Boolean }

-- the version the page should start on: "" the album as opened, "tidal" its Tidal album (a
-- record's or a file album's), "J…" one of its file albums. Only what ▶ can play takes
-- part: a Tidal album not gone from Tidal, files whose folder the player has. A record is not
-- played: on a record's page the candidates are its Tidal album and its files only, and ""
-- means none.
bestVersion :: Album -> AlbumXJ -> String
bestVersion a x =
  let kind = take 1 a.albumID
      tidalQ = x.xfacts.afQuality
      rankOf m = fromMaybe 0 (map qualityRank m)
      hasTidal = fromMaybe "" a.albumTidal /= "" && not (elem "tidal.gone" a.albumTags)
      own = case kind of
        "T" -> [ { key: "", rank: rankOf tidalQ, tidal: true, own: true } ]
        "J" -> if fromMaybe true x.xnode then [ { key: "", rank: rankOf x.xquality, tidal: false, own: true } ] else []
        _ -> []
      twin = if kind /= "T" && hasTidal then [ { key: "tidal", rank: rankOf tidalQ, tidal: true, own: false } ] else []
      files = mapMaybe (\f -> if take 1 f.fvKey == "J" && fromMaybe true f.fvNode then Just { key: f.fvKey, rank: rankOf f.fvQuality, tidal: false, own: false } else Nothing)
                       (fromMaybe [] x.xfiles)
      score c = c.rank * 4 + (if c.tidal then 2 else 0) + (if c.own then 1 else 0)
      better :: Maybe Cand -> Cand -> Maybe Cand
      better acc c = case acc of
        Just b | score b >= score c -> Just b
        _ -> Just c
  in maybe "" _.key (foldl better Nothing (own <> twin <> files))
  where
  maybe d f m = case m of
    Just v -> f v
    Nothing -> d
