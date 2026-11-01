module Main (main) where

import qualified Data.ByteString as BS
import Data.Time (TimeZone (..), ZonedTime (..), LocalTime (..), TimeOfDay (..), fromGregorian)
import Listened
import Relude
import System.Directory (doesDirectoryExist, getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import Types (Album (..), TagFolder (..))

album :: Album
album =
  Album
    { albumID = 6715388
    , albumInst = 2200131655
    , albumTitle = "The Legacy Of Maria Yudina - Volume 6"
    , albumArtist = "Maria Yudina, \"Franz\" Schubert \\ Schumann"
    , albumReleased = ""
    , albumCover = ""
    , albumAdded = ""
    , albumFolder = 3292597
    , albumURL = ""
    , albumFormat = "CD"
    , albumTidal = Just "283130328"
    , albumAMusic = Just "1556465220"
    , albumQobuz = Nothing
    , albumJellyfin = Nothing
    , albumLocIdx = Nothing
    , albumLocation = Nothing
    , albumTags = []
    , albumRating = 0
    , albumPlays = 2
    }

tidalAlbum :: Album
tidalAlbum =
  album
    { albumID = 187293380
    , albumInst = 0
    , albumFolder = fromEnum TTidal
    , albumFormat = "Tidal"
    , albumTidal = Just "187293380"
    , albumAMusic = Nothing
    }

now :: ZonedTime
now =
  ZonedTime
    (LocalTime (fromGregorian 2026 11 1) (TimeOfDay 10 58 0))
    (TimeZone (-360) False "CST")

check :: String -> Bool -> IO Bool
check name ok = do
  putStrLn $ (if ok then "ok   " else "FAIL ") <> name
  pure ok

main :: IO ()
main = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "clutter-listened-test"
  whenM (doesDirectoryExist dir) $ removeDirectoryRecursive dir
  let e1 = playEvent now album
      e2 = playEvent now tidalAlbum
  fn <- appendPlay dir e1
  _ <- appendPlay dir e2
  r1 <- readPlays dir
  -- simulate a crash mid-write, then append once more
  appendFileBS fn "{\"ts\":\"2026-11-01T11:"
  r2 <- readPlays dir
  _ <- appendPlay dir e2
  r3 <- readPlays dir
  raw <- readFileBS fn
  oks <-
    sequence
      [ check "file is listened/2026.jsonl" (fn == dir </> "2026.jsonl")
      , check "ts carries the UTC offset" (peTs e1 == "2026-11-01T10:58:00-06:00")
      , check "discogs key and ids" (albumKey album == KDiscogs 6715388 && peDiscogs e1 == Just 6715388 && peInst e1 == Just 2200131655 && peTidal e1 == Just 283130328 && peApple e1 == Just 1556465220)
      , check "tidal key, no discogs id" (albumKey tidalAlbum == KTidal 187293380 && peSrc e2 == "tidal" && isNothing (peDiscogs e2) && isNothing (peInst e2))
      , check "round trip" (r1 == [e1, e2])
      , check "torn last line skipped" (r2 == [e1, e2])
      , check "append after torn line survives" (r3 == [e1, e2, e2])
      , check "one event per line" (length (BS.split 10 raw) == 5)
      , check "Nothing fields omitted" (not ("\"note\"" `BS.isInfixOf` raw))
      ]
  removeDirectoryRecursive dir
  if and oks then exitSuccess else exitFailure
