{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module App (
  startApp,
  app,
  appWith,
)
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Numeric (showHex)
import Jellyfin (jellyfinPaths)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import System.IO.Unsafe (unsafePerformIO)
import Data.Time (ZonedTime, defaultTimeLocale, formatTime, getZonedTime)

import Control.Exception (try)
import Control.Monad (foldM)
import qualified Data.Char as Ch
import qualified Data.List as L (isInfixOf, nubBy)
import qualified Data.Text as T (length, map, null, strip, words)
import qualified Data.Set as Set
import qualified Data.Map.Strict as M
import Data.Text as T (breakOn, isInfixOf, isSuffixOf, stripPrefix, takeWhile, unlines, unpack)
import qualified Network.HTTP.Client as HC
import Data.Text.IO as TIO (writeFile)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as V
import Env (
  envGetTag,
  envInit,
  envRefreshPlays,
  envReloadLog,
  envTidalConnect,
  envTidalReload,
  envJellyfinReload,
  envUpdate,
  envUpdateAlbum,
  envGetAlbum,
  envRefreshAlbum,
  envUpdateMenuParams,
  envUpdateSort,
 )
import Collection (appendLead, appendMark, appendMerge, appendNotSame, appendSkip, appendSplit, appendVersion, collectionFile, followMove, newAlbumId, notSame, parseStreamRef, readMoves, reviewedFiles, sameKind, standInFolderTags)
import qualified Data.Text as TX
import Listened (appendPlay, listenedDir, listenedName, listenedRows, playAlbum, playEvent)
import Related (Related (..), noRelated, related)
import Tracks (AlbumTracks, Want (..), albumTracks)
import qualified Lucid as L
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Relude
import Render (renderAlbumJournal, renderAlbumText, renderAlbumView, renderAlbumsView, renderApp)
import Servant
import System.Directory (doesFileExist, makeAbsolute)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hPutStrLn)
import System.Process (rawSystem, readProcess, readProcessWithExitCode)
import Types (
  Album (..),
  AlbumFacts (..),
  AlbumKey (..),
  AppM,
  Discogs (..),
  Env (..),
  EnvR (..),
  Layer (..),
  MenuParams (..),
  PlayEvent (..),
  envGetDiscogs,
  envGetEnvr,
  nameOf,
  pLocList,
  parseKey,
  rowOf,
  rowOfName,
  versionsOfKey,
 )

import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecode, encode, object, withObject, (.!=), (.:?), (.=))

newtype WantsJ = WantsJ {wnotes :: Text} deriving (Show, Generic)
instance FromJSON WantsJ where
  parseJSON = withObject "wants" $ \o -> do
    wnotes_ <- o .:? "notes" .!= ""
    pure $ WantsJ wnotes_

data HTML

newtype RawHtml = RawHtml {unRaw :: BL.ByteString}

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

------------- Clutter API defined in Servant

type API0 =
  "album"
    :> Capture "aid" AlbumKey
    :> Get '[HTML] RawHtml

type API1 =
  "albums"
    :> Capture "list" Text
    :> QueryParam "sortBy" Text
    :> QueryParam "sortOrder" Text
    :> QueryParams "focus" Text
    :> Get '[HTML] RawHtml

type API2 =
  "provider"
    :> "discogs"
    :> QueryParam "token" Text
    :> QueryParam "username" Text
    :> QueryParam "nreleases" Int -- update last n releases, all if null
    :> QueryParam "release" Int -- get another release into the database
    :> Get '[HTML] RawHtml

type API3 =
  "provider"
    :> "tidal"
    :> QueryParam "nalbums" Int -- update last n tidal albums, all if null
    :> Get '[HTML] RawHtml

type API4 =
  "api"
    :> "albumq"
    :> Capture "aid" AlbumKey
    :> Get '[JSON] AlbumJ
    -- a Discogs release asked again after its page is drawn: the album if Discogs changed it
    :<|> "api"
      :> "albumrefresh"
      :> Capture "aid" AlbumKey
      :> Get '[JSON] AlbumJ

type API5 =
  "api"
    :> "albumsq"
    :> Capture "list" Text
    :> QueryParam "sortBy" Text
    :> QueryParam "sortOrder" Text
    :> QueryParams "focus" Text
    :> Get '[JSON] AlbumsJ

type API6 =
  "api"
    :> "paramsq"
    :> "all"
    :> Get '[JSON] ParamsJ

type API7 =
  "api"
    :> "req"
    :> QueryParam "event" Text
    :> Get '[JSON] ReqJ

type API8 =
  "api"
    :> "albump"
    :> Capture "aid" AlbumKey
    :> Get '[JSON] AlbumJ

type API9 =
  "app"
    :> Get '[HTML] RawHtml

-- the play log, one row per play: "all" or a year
type API10 =
  "api"
    :> "listenedq"
    :> Capture "year" Text
    :> QueryParam "sortBy" Text
    :> QueryParam "sortOrder" Text
    :> QueryParams "focus" Text
    :> Get '[JSON] ListenedJ

-- play a Tidal album on the Bluesound Node through its BluOS API (port 11000)
-- mode: replace (default) | now | next | last
type API11 =
  "api"
    :> "nodeplay"
    :> Capture "tid" Int
    :> QueryParam "mode" Text
    :> Get '[JSON] ReqJ

-- Node transport: toggle (pause/resume) | pause | play | stop
type API12 =
  "api"
    :> "nodectl"
    :> Capture "cmd" Text
    :> Get '[JSON] ReqJ

-- the Node's player name from /SyncStatus (e.g. "lblue") in reqInfo
type API13 =
  "api"
    :> "nodeinfo"
    :> Get '[JSON] ReqJ

-- the album page's details (docs/album-page-plan.md, phase 1): facts from the stores and
-- the album strips. Asked for when an album is opened, so that Album stays small in lists.
type API14 =
  "api"
    :> "albumxq"
    :> Capture "aid" AlbumKey
    :> Get '[JSON] AlbumXJ

data AlbumXJ = AlbumXJ
  { xid :: AlbumKey
  , xfacts :: AlbumFacts
  , xrelated :: Related
  , xloc :: Maybe (Text, Int) -- where the physical album is: location list and position in it
  , xfiles :: [FileV] -- the file albums on the server that are versions of this album
  , xcandidates :: [CandV] -- for a file album: the albums it may be a version of, best first
  , xstreams :: [AlbumKey] -- the Tidal / Apple albums this release has from the collection log (they can be taken off here)
  , xnode :: Bool -- the album's files (its own, or its first file version's) are in a share the Node has indexed
  , xof :: Maybe CandV -- a version that is not its album's lead: the album's row (its lead version)
  , xnotsame :: [CandV] -- suggestions marked "not the same album" (×), which ↺ can bring back
  , xversions :: [CandV] -- every other version of the album, lead first (a stream clutter does not hold: its key alone)
  , xlead :: AlbumKey -- the album's lead version (its row)
  , xleadSet :: Bool -- a lead line chose it, not the rule
  , xmembers :: [AlbumKey] -- the versions the page may take off or let lead (membersOf): none for an album of one version
  }
  deriving (Show, Generic)
instance ToJSON AlbumXJ

-- an album a file album may be a version of (docs/jellyfin-plan.md, linking by hand)
data CandV = CandV
  { cvKey :: AlbumKey
  , cvTitle :: Text
  , cvArtist :: Text
  , cvFormat :: Text
  , cvReleased :: Text
  , cvHas :: Text -- for a stream album: the album of that service the release has now ("T235035298"), "" if none
  }
  deriving (Show, Generic)
instance ToJSON CandV

-- a file album as a version of another album (docs/jellyfin-plan.md)
data FileV = FileV
  { fvKey :: AlbumKey
  , fvTitle :: Text
  , fvArtist :: Text
  , fvStorage :: Text -- storage location, "" if none
  , fvUrl :: Text -- the album in Jellyfin, "" if Jellyfin's address is not known
  , fvLogged :: Bool -- linked in the collection log (can be taken off here); False: named in the Discogs Location field
  , fvOwner :: Maybe AlbumKey -- the other release of this album that holds the link (same Discogs master); Nothing: this one
  , fvOwnerFormat :: Text -- that release's format, "Vinyl"
  }
  deriving (Show, Generic)
instance ToJSON FileV

-- tracks, credits and review of an album (docs/album-page-plan.md, phase 2): fetched from
-- Discogs (a physical release) or Tidal (a stream) when first asked for, then from the
-- per-album cache; refresh=1 refetches, source=tidal asks for a release's Tidal version
type API15 =
  "api"
    :> "albumtq"
    :> Capture "aid" AlbumKey
    :> QueryParam "refresh" Text
    :> QueryParam "source" Text -- "tidal": the Tidal version of a Discogs release
    :> Get '[JSON] AlbumTJ

data AlbumTJ = AlbumTJ
  { tjId :: AlbumKey
  , tjTracks :: Maybe AlbumTracks
  , tjError :: Text -- why there are none, "" otherwise
  }
  deriving (Show, Generic)
instance ToJSON AlbumTJ

-- remove a stream album from clutter (docs/album-page-plan.md): marked "dropped" in the Tidal
-- album store, by the script that owns the store, and left out from then on; its plays stay
type API16 =
  "api"
    :> "albumdrop"
    :> Capture "aid" AlbumKey
    :> Get '[JSON] ReqJ

-- give a Discogs release its Tidal or Apple Music album by hand, or take that off again
-- (docs/jellyfin-plan.md): /api/albumstream/D375065?ref=<T…, A… or a share link>&op=link|unlink,
-- appended to the collection log. Not a write to Discogs: the Location field stays as it is.
type API19 =
  "api"
    :> "albumstream"
    :> Capture "aid" AlbumKey
    :> QueryParam "ref" Text
    :> QueryParam "op" Text
    :> Get '[JSON] ReqJ

-- make a file album a version of an album, or take that off (docs/jellyfin-plan.md):
-- /api/albumlink/D1365907?version=J32aa…&op=link|unlink, appended to the collection log
type API18 =
  "api"
    :> "albumlink"
    :> Capture "aid" AlbumKey
    :> QueryParam "version" AlbumKey
    :> QueryParam "op" Text
    :> Get '[JSON] ReqJ

-- the keep mark of an album (docs/jellyfin-plan.md): value = yes | no | unset, appended to
-- clutter's collection log; the collection.* tags and the Collection and Inbox lists follow
type API17 =
  "api"
    :> "albumkeep"
    :> Capture "aid" AlbumKey
    :> QueryParam "value" Text
    :> Get '[JSON] ReqJ

-- the review of the inventory's unsure matches (docs/jellyfin-plan.md, "Review of the
-- unsure matches"): the file albums scripts/jellyfin-inventory.py could not link for sure,
-- each with the albums it may be a version of. Link with /api/albumlink; none of them:
type API20 =
  "api"
    :> "reviewq"
    :> Get '[JSON] ReviewJ

-- /api/albumskip/J32aa…: none of the albums offered is this file album; logged, not offered again
type API21 =
  "api"
    :> "albumskip"
    :> Capture "aid" AlbumKey
    :> Get '[JSON] ReqJ
    -- × under "Same album as…": the suggestion cand is not the same album; op=undo takes it back
    :<|> "api"
      :> "albumnotsame"
      :> Capture "aid" AlbumKey
      :> QueryParam "cand" AlbumKey
      :> QueryParam "op" Text
      :> Get '[JSON] ReqJ

-- the keep mark for many albums at once (bulk Keep in a list): POST /api/albumskeep?value=
-- yes | no | unset, the album keys as a JSON array in the body. One log line per album
-- whose mark changes, one recomputation of the tags.
type API22 =
  "api"
    :> "albumskeep"
    :> QueryParam "value" Text
    :> ReqBody '[JSON] [AlbumKey]
    :> Post '[JSON] ReqJ

-- play an album's files on the Bluesound Node, from the player's own library (the shares
-- it has indexed): /api/nodeplayfiles/:id?mode=replace|now|next|last[&version=J…]. The id
-- is a file album, or an album that has file versions (the first, or the one named).
type API23 =
  "api"
    :> "nodeplayfiles"
    :> Capture "aid" AlbumKey
    :> QueryParam "mode" Text
    :> QueryParam "version" AlbumKey
    :> Get '[JSON] ReqJ

-- the album layer by hand (docs/album-layer-plan.md › Slices, 3):
--   /api/albumjoin/:aid?other=K     K's album and aid's are one album (version, merge lines)
--   /api/albumsplit/:aid?version=K  K, a version of aid's album, is its own album again
--   /api/albumlead/:aid?version=K   K leads aid's album; without version: back to the rule
--   /api/albumfind/:aid?q=words     any album row whose title and artist have all the
--                                   words, for "Same album as…" (not aid's own album)
type API24 =
  "api"
    :> "albumjoin"
    :> Capture "aid" AlbumKey
    :> QueryParam "other" AlbumKey
    :> Get '[JSON] RowJ
    :<|> "api"
      :> "albumsplit"
      :> Capture "aid" AlbumKey
      :> QueryParam "version" AlbumKey
      :> Get '[JSON] RowJ
    :<|> "api"
      :> "albumlead"
      :> Capture "aid" AlbumKey
      :> QueryParam "version" AlbumKey
      :> Get '[JSON] RowJ
    :<|> "api"
      :> "albumfind"
      :> Capture "aid" AlbumKey
      :> QueryParam "q" Text
      :> Get '[JSON] [CandV]

-- the answer to a change of the album layer: as ReqJ, and the key of the album's row
-- afterwards (the page to show)
data RowJ = RowJ
  { rjTime :: Text
  , rjInfo :: Text
  , rjResult :: Int
  , rjRow :: AlbumKey
  }
  deriving (Show, Generic)
instance ToJSON RowJ

data ReviewJ = ReviewJ
  { rvRows :: [ReviewRow]
  , rvDone :: Int -- the inventory's unsure rows already decided (linked, unlinked or skipped)
  , rvError :: Text -- why there is no list, "" otherwise
  }
  deriving (Show, Generic)
instance ToJSON ReviewJ

-- a file album to review and the albums it may be a version of, the inventory's choice first
data ReviewRow = ReviewRow
  { rrKey :: AlbumKey
  , rrTitle :: Text
  , rrArtist :: Text
  , rrYear :: Text
  , rrMins :: Text
  , rrStorage :: Text -- storage location, "" if none
  , rrFolder :: Text -- the album's folder on the server, its last part
  , rrUrl :: Text -- the album in Jellyfin
  , rrWhy :: Text -- why it was not linked for sure
  , rrCands :: [CandV]
  }
  deriving (Show, Generic)
instance ToJSON ReviewRow

data AlbumJ = AlbumJ
  { aid :: AlbumKey
  , album :: Maybe Album
  }
  deriving (Eq, Show, Generic)
instance ToJSON AlbumJ

data AlbumsJ = AlbumsJ
  { listName :: Text
  , -- , lalbums :: [Album]
    lalbums :: [(Album, Maybe (Text, Int))]
  }
  deriving (Eq, Show, Generic)
instance ToJSON AlbumsJ

-- AlbumsJ plus, row by row, the time of that play (same JSON keys as AlbumsJ)
data ListenedJ = ListenedJ
  { ljName :: Text
  , ljAlbums :: [(Album, Maybe (Text, Int))]
  , ljPlayed :: [Text]
  }
  deriving (Eq, Show, Generic)
instance ToJSON ListenedJ where
  toJSON l = object ["listName" .= ljName l, "lalbums" .= ljAlbums l, "lplayed" .= ljPlayed l]

data ParamsJ = ParamsJ
  { timeStamp :: Text
  , params :: MenuParams
  }
  deriving (Eq, Show, Generic)
instance ToJSON ParamsJ

data ReqJ = ReqJ
  { reqTime :: Text
  , reqInfo :: Text
  , reqResult :: Int
  }
  deriving (Eq, Show, Generic)
instance ToJSON ReqJ

type ClutterAPI =
  API0
    :<|> API1
    :<|> API2
    :<|> API3
    :<|> API4
    :<|> API5
    :<|> API6
    :<|> API7
    :<|> API8
    :<|> API9
    :<|> API10
    :<|> API11
    :<|> API12
    :<|> API13
    :<|> API14
    :<|> API15
    :<|> API16
    :<|> API17
    :<|> API18
    :<|> API19
    :<|> API20
    :<|> API21
    :<|> API22
    :<|> API23
    :<|> API24
    :<|> Raw

clutterAPI :: Proxy ClutterAPI
clutterAPI = Proxy

-- add "Middleware", see
-- https://mmhaskell.com/blog/2018/10/8/stuck-in-the-middle-adding-middleware-to-a-servant-server
-- where we modify the response header such that it includes CORS header Access-Control-Allow-Origin: *
--
addAllOriginsMiddleware :: Application -> Application
addAllOriginsMiddleware baseApp req responseFunc = baseApp req (responseFunc . addOriginsAllowed)

-- addAllOriginsMiddleware baseApp = \req responseFunc -> baseApp req (responseFunc . addOriginsAllowed)

addOriginsAllowed :: Response -> Response
addOriginsAllowed =
  mapResponseHeaders $
    (:) ("Access-Control-Allow-Origin", "*")

---
--- keep a rebuilt frontend bundle (or stylesheet) from being served out of a stale
--- browser cache. With no Cache-Control at all the browser is free to invent a
--- freshness lifetime from the file's age (~10% of it), so weeks after a build a
--- plain reload can still run the old index.js. "no-cache" does not disable the
--- cache, it only forces revalidation -- and the static file server answers
--- If-Modified-Since with a 304, so the cost is one conditional request.
---
addNoCacheMiddleware :: Application -> Application
addNoCacheMiddleware baseApp req responseFunc =
  baseApp req (responseFunc . if isAssetPath (pathInfo req) then addNoCache else id)

isAssetPath :: [Text] -> Bool
isAssetPath ps = case reverse ps of
  (p : _) -> any (`T.isSuffixOf` p) [".js", ".css"]
  [] -> False

addNoCache :: Response -> Response
addNoCache =
  mapResponseHeaders $
    (:) ("Cache-Control", "no-cache")

-- (:) ("Content-Security-Policy", "navigate-to * ")

-- type AppM = ReaderT Env Handler defined in Types.hs
nt :: Env -> (ReaderT Env) Handler a -> Handler a
nt env x = x `runReaderT` env

app :: Env -> Application
app = appWith "static"

appWith :: FilePath -> Env -> Application
appWith sd env = addAllOriginsMiddleware . addNoCacheMiddleware $ serve clutterAPI (hoistServer clutterAPI (`runReaderT` env) (clutterServer sd))

clutterServer :: FilePath -> ServerT ClutterAPI AppM
clutterServer sd =
  serveAlbum
    :<|> serveAlbums
    :<|> serveDiscogs
    :<|> serveTidal
    :<|> (serveAlbumq :<|> serveAlbumrefresh)
    :<|> serveAlbumsq
    :<|> serveParamsq
    :<|> serveReq
    :<|> serveAlbump
    :<|> serveApp
    :<|> serveListenedq
    :<|> serveNodeplay
    :<|> serveNodectl
    :<|> serveNodeinfo
    :<|> serveAlbumxq
    :<|> serveAlbumtq
    :<|> serveAlbumdrop
    :<|> serveAlbumkeep
    :<|> serveAlbumlink
    :<|> serveAlbumstream
    :<|> serveReviewq
    :<|> (serveAlbumskip :<|> serveAlbumnotsame)
    :<|> serveAlbumskeep
    :<|> serveNodeplayfiles
    :<|> (serveAlbumjoin :<|> serveAlbumsplit :<|> serveAlbumlead :<|> serveAlbumfind)
    :<|> serveDirectoryFileServer sd
 where
  decodeListQuery :: Text -> Maybe Text -> Maybe Text -> [Text] -> AppM (V.Vector AlbumKey)
  decodeListQuery ln msb mso fs = do
    -- listName sortBy sortOrder focusItems
    env <- ask
    liftIO $ print ("-------decodeListQuery " :: Text, ln, msb, mso, fs)

    -- get ids from either list or tags (#tag)
    ly <- readIORef (layerR env)
    keys <- case T.stripPrefix "#" ln of
      Just tag -> envGetTag tag
      Nothing -> V.toList <$> getList env ln
    -- A shelf list's rows are records, each with its own tags (Collection.shelfIndexTags).
    -- Every other list shows an album once, as its row (its lead version), which matches
    -- on the tags of all its versions; a list of plays (Listened) has a row per play.
    let shelf = pLocList ln
        aids'
          | shelf = keys
          | isJust (listenedName ln) = map (rowOf ly) keys
          | otherwise = ordNub (map (rowOf ly) keys)
    ---------------------------------------------------------------------------------
    -- filter with focus if any
    aidset <- focusFilterIn (if shelf then shelfTagsR env else tagsR env) fs (Set.fromList aids')
    let aids = V.mapMaybe (\i -> if i `Set.member` aidset then Just i else Nothing) . V.fromList $ aids'
    ---------------------------------------------------------------------------------
    envr <- envUpdateSort msb mso
    let EnvR am _ _ _ _ sn so _ _ _ = envr
    let doSort = getSort env am sn
    pure . doSort so $ aids

  -- narrow a set of album ids by focus items: #tag keeps, -#tag drops
  focusFilter :: [Text] -> Set.Set AlbumKey -> AppM (Set.Set AlbumKey)
  focusFilter fs aidset' = asks tagsR >>= \r -> focusFilterIn r fs aidset'

  -- the same with a tags index: an album's row (tagsR) or a record's in a shelf (shelfTagsR)
  focusFilterIn :: IORef (Map Text [AlbumKey]) -> [Text] -> Set.Set AlbumKey -> AppM (Set.Set AlbumKey)
  focusFilterIn index fs aidset' = do
    tm <- readIORef index
    foldM
      ( \s (t, p) ->
          pure
            . ( if p
                  then Set.intersection s
                  else Set.difference s
              )
            . Set.fromList
            $ M.findWithDefault [] t tm
      )
      aidset'
      . map
        ( \t ->
            ( fromMaybe t (T.stripPrefix "-" t)
            , isNothing (T.stripPrefix "-" t)
            )
        )
      . mapMaybe (T.stripPrefix "#")
      $ fs

  -- every play of a year ("all": of every year). Sorted like the other lists: Default is
  -- the list's own order, newest play first; Artist, Title, Added sort the rows by their
  -- album (stable, so plays of one album stay newest first); sortOrder=Desc reverses.
  -- A play of an album clutter does not hold gets one made up from the play, and is
  -- dropped by any focus filter
  serveListenedq :: Text -> Maybe Text -> Maybe Text -> [Text] -> AppM ListenedJ
  serveListenedq y msb mso fs = do
    liftIO . putTextLn $ "-------serveListenedq " <> y
    env <- ask
    es <- readIORef (playsR env)
    ly <- readIORef (layerR env)
    envr <- envGetEnvr
    let my = if y == "all" then Nothing else Just y
        rows = listenedRows my es (fmap (rowOfName ly) . lyPlay ly) (albums envr)
        shown = [(e, a) | (e, Just a) <- rows]
    keep <- focusFilter fs (Set.fromList (albumID . snd <$> shown))
    let rows' =
          [ (e, fromMaybe (playAlbum e) ma)
          | (e, ma) <- rows
          , null fs || maybe False ((`Set.member` keep) . albumID) ma
          ]
        sorted = case msb of
          Just "Artist" -> sortOn (albumArtist . snd) rows'
          Just "Title" -> sortOn (albumTitle . snd) rows'
          Just "Added" -> sortOn (Down . albumAdded . snd) rows'
          _ -> rows'
        ordered = if mso == Just "Desc" then reverse sorted else sorted
    pure
      ListenedJ
        { ljName = if y == "all" then "Listened" else "Listened " <> y
        , ljAlbums = [(a, albumID a `M.lookup` locs envr) | (_, a) <- ordered]
        , ljPlayed = peTs . fst <$> ordered
        }

  serveAlbumsq ::
    Text ->
    Maybe Text ->
    Maybe Text ->
    [Text] ->
    AppM AlbumsJ
  serveAlbumsq ln msb mso fs = do
    liftIO . putTextLn $ "-------serveAlbumsq " <> ln
    aids <- decodeListQuery ln msb mso fs
    envr <- envGetEnvr
    -- gl <- asks getList
    -- aids <- gl ln
    -- let la = mapMaybe (`M.lookup` albums envr) . V.toList $ aids
    let la =
          mapMaybe
            ( \aid' -> case (aid' `M.lookup` albums envr, aid' `M.lookup` locs envr) of
                (Nothing, _) -> Nothing
                (Just a, b) -> Just (a, b)
            )
            . V.toList
            $ aids
    liftIO . putTextLn $ "-------serveAlbumsq: first 3 elements from [(album, locs)] of list " <> ln
    liftIO . print . take 3 $ la
    let asj = AlbumsJ{listName = ln, lalbums = la}
    pure asj

  -- the album as clutter holds it, at once (slice 18): Discogs is asked afterwards, by
  -- the page, through /api/albumrefresh
  serveAlbumq :: AlbumKey -> AppM AlbumJ
  serveAlbumq i = do
    liftIO $ print ("-------serveAlbumq " :: Text, i)
    ma <- envGetAlbum i
    let aj = case ma of
          Just a -> AlbumJ{aid = i, album = Just a}
          _ -> AlbumJ{aid = i, album = Nothing}
    pure aj

  serveAlbumrefresh :: AlbumKey -> AppM AlbumJ
  serveAlbumrefresh i = AlbumJ i <$> envRefreshAlbum i

  serveAlbumxq :: AlbumKey -> AppM AlbumXJ
  serveAlbumxq i = do
    am <- asks albumsR >>= readIORef
    ly <- asks layerR >>= readIORef
    facts <- asks albumFactsR >>= readIORef
    lcs <- asks locsR >>= readIORef
    onNode <- liftIO $ maybe (pure False) (fmap isJust . nodeFolder) (M.lookup i am >>= fileKey)
    -- the suggestions marked "not the same album" (×) for this one
    excl <- liftIO $ M.findWithDefault [] i <$> (collectionFile >>= notSame)
    sit <- liftIO standInFolderTags
    -- a file album is named by its storage location ("Files · 1001"), not "Jellyfin": the
    -- page says which copy a version or the album's lead is
    let fmtOf b
          | isFileKey (albumID b) = "Files" <> maybe "" (" · " <>) (viaNonEmpty head (mapMaybe (T.stripPrefix "storage.") (albumTags b)))
          | otherwise = albumFormat b
        candV b = CandV{cvKey = albumID b, cvTitle = albumTitle b, cvArtist = albumArtist b, cvFormat = fmtOf b, cvReleased = albumReleased b, cvHas = ""}
        others = filter (/= i) (versionsOfKey ly i)
        -- a version clutter does not hold (a stream that is no favourite): its key alone
        versionV k = maybe CandV{cvKey = k, cvTitle = "", cvArtist = "", cvFormat = "", cvReleased = "", cvHas = ""} candV (M.lookup k am)
        -- a version that is not its album's lead: the album, with all its versions, is its lead's row
        versionOf = let r = rowOf ly i in if r /= i then candV <$> M.lookup r am else Nothing
    pure $ case M.lookup i am of
      Just a -> AlbumXJ{xnode = onNode, xid = i, xfacts = facts a, xrelated = related am a, xloc = M.lookup i lcs, xfiles = filesOf ly am a, xcandidates = filter ((`notElem` excl) . cvKey) (candidatesFor sit ly am a), xstreams = filter isStreamKey others, xof = versionOf, xnotsame = mapMaybe (fmap candV . (`M.lookup` am)) excl, xversions = map versionV others, xlead = rowOf ly i, xleadSet = Set.member (nameOf ly i) (lyLeadSet ly), xmembers = let ms = membersOf ly i in if length ms > 1 then ms else []}
      -- not in the collection (an album made up from a play): no details
      Nothing -> AlbumXJ{xnode = False, xid = i, xfacts = AlbumFacts Nothing Nothing Nothing Nothing Nothing Nothing False [] Nothing, xrelated = noRelated, xloc = Nothing, xfiles = [], xcandidates = [], xstreams = [], xof = Nothing, xnotsame = [], xversions = [], xlead = i, xleadSet = False, xmembers = []}

  serveAlbumtq :: AlbumKey -> Maybe Text -> Maybe Text -> AppM AlbumTJ
  serveAlbumtq i mrefresh msource = do
    am <- asks albumsR >>= readIORef
    case M.lookup i am of
      Nothing -> pure AlbumTJ{tjId = i, tjTracks = Nothing, tjError = "not in the collection"}
      Just a -> do
        let want = case msource of
              Just "tidal" -> WantTidal
              Just "files" -> WantFiles
              _ -> Own
        r <- liftIO $ albumTracks (isJust mrefresh) want a
        pure $ case r of
          Right t -> AlbumTJ{tjId = i, tjTracks = Just t, tjError = ""}
          Left err -> AlbumTJ{tjId = i, tjTracks = Nothing, tjError = err}

  serveAlbumdrop :: AlbumKey -> AppM ReqJ
  serveAlbumdrop i = do
    now <- liftIO getZonedTime
    let dtl = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" now
        answer ok info = ReqJ{reqTime = dtl, reqInfo = info, reqResult = if ok then 1 else 0}
    am <- asks albumsR >>= readIORef
    case (i, M.lookup i am) of
      (T n, Just a) -> do
        ran <- liftIO . try $ readProcessWithExitCode "python3" ["scripts/update-tidal-albums.py", "--drop", show n] ""
        case ran :: Either SomeException (ExitCode, String, String) of
          Right (ExitSuccess, _, _) -> do
            _ <- envTidalReload
            liftIO $ hPutStrLn stderr ("removed from clutter: " <> show i)
            pure $ answer True ("Removed “" <> albumTitle a <> "” from clutter")
          _ -> pure $ answer False "Could not remove the album (the store was not changed)"
      (T _, Nothing) -> pure $ answer False "Not in clutter"
      -- a Discogs release leaves clutter when it leaves the Discogs collection
      _ -> pure $ answer False "Only a stream album can be removed here"

  -- the file versions of an album that clutter holds, other than the one the page is
  -- (albumFiles: Collection.attachAlbums); each can be taken off the album (×: a split)
  filesOf :: Layer -> Map AlbumKey Album -> Album -> [FileV]
  filesOf ly am a = [fileV j f | j <- albumFiles a, Just f <- [M.lookup (J j) am]]
   where
    inAlbum j = J j `elem` versionsOfKey ly (albumID a)
    fileV j f =
      FileV
        { fvKey = J j
        , fvTitle = albumTitle f
        , fvArtist = albumArtist f
        , fvStorage = fromMaybe "" (viaNonEmpty head (mapMaybe (T.stripPrefix "storage.") (albumTags f)))
        , fvUrl = albumURL f
        , fvLogged = inAlbum j
        , fvOwner = Nothing
        , fvOwnerFormat = ""
        }

  isRecordKey :: AlbumKey -> Bool
  isRecordKey k = case k of
    D _ -> True
    _ -> False

  isFileKey :: AlbumKey -> Bool
  isFileKey k = case k of
    J _ -> True
    _ -> False

  isStreamKey :: AlbumKey -> Bool
  isStreamKey k = case k of
    T _ -> True
    A _ -> True
    Q _ -> True
    _ -> False

  notAlbumFormat :: Text
  notAlbumFormat = "The collection log is not in the album format yet (scripts/album-layer-migrate.py --write): nothing was linked"

  -- the album a key is a version of, held or not (a stream that is no favourite may be a
  -- version in the log): its C id, or the key itself
  nameAny :: Layer -> AlbumKey -> AlbumKey
  nameAny ly k
    | M.member k (lyName ly) = nameOf ly k
    | otherwise = maybe k fst (find ((k `elem`) . snd) (M.toList (lyVersions ly)))

  -- The lines that join key o's album to key k's (docs/album-layer-plan.md › Log lines):
  -- one version line into an album with an id; a new id and two version lines for two
  -- albums of one version each; a merge (into k's) for two with ids. alone: o leaves an
  -- album it was a version of (a split) and joins by itself, instead of bringing it along.
  joinLines :: ZonedTime -> Layer -> Bool -> AlbumKey -> AlbumKey -> Album -> FilePath -> IO ()
  joinLines now ly alone k o a fn = do
    let nk = nameAny ly k
        no0 = nameAny ly o
        isId n = case n of
          C _ -> True
          _ -> False
    no <-
      if alone && isId no0 && no0 /= nk
        then appendSplit fn now o a $> o
        else pure no0
    case () of
      _
        | nk == no -> pure ()
        | isId nk && isId no -> appendMerge fn now no nk a
        | isId nk -> appendVersion fn now nk no a
        | isId no -> appendVersion fn now no nk a
        | otherwise -> do
            let c = newAlbumId now [nk, no] (lyIds ly <> Set.fromList (M.keys (lyVersions ly)))
            appendVersion fn now c nk a
            appendVersion fn now c no a

  -- For a file album: the releases and stream albums it may be a version of, by title --
  -- the same, or one title inside the other ("Mind Maps 6" in "FSOLdigital Presents Mind
  -- Maps 6"), or most words shared -- with a shared artist word breaking ties. The automatic
  -- import (scripts/import-inventory-links.py) only takes exact pairs; these are for a click.
  candidatesFor :: Set Text -> Layer -> Map AlbumKey Album -> Album -> [CandV]
  --
  -- Likewise for a stream album: the albums it may be a version of (a release, a file
  -- album, another edition of the stream); and for a record, other records of the same
  -- title and artist (a second pressing, the CD of a vinyl). Since slice 3 of the album
  -- layer a suggestion joins the two albums (/api/albumjoin); nothing is replaced.
  -- Only for an album's row, and never an album it is a version of already.
  candidatesFor sit ly am a
    | hidden (albumID a) = []
    | otherwise =
        [ CandV{cvKey = albumID b, cvTitle = albumTitle b, cvArtist = albumArtist b, cvFormat = albumFormat b, cvReleased = albumReleased b, cvHas = has b}
        | (_, b) <- take 8 (sortOn (Down . fst) scored)
        ]
   where
    -- the stream of that service the other album has already (it stays: a second edition)
    -- (not for a Tidal or Apple album itself: its own id)
    has b = case (albumID a, albumID b) of
      (_, T _) -> ""
      (_, A _) -> ""
      (T _, _) -> maybe "" ("T" <>) (mfilter (not . T.null) (albumTidal b))
      (A _, _) -> maybe "" ("A" <>) (mfilter (not . T.null) (albumAMusic b))
      _ -> ""
    -- what the album can be joined to: a record only to an album with a record of the same
    -- Discogs master (by title, an opera's other recordings would all look the same:
    -- Carmen, Aida); anything else to anything
    fits b = case (albumID a, albumID b) of
      -- (not a stand-in release of the folders Streaming and Files: it is no version, slice 6)
      (D _, D _) -> not (Set.null (masters a `Set.intersection` masters b)) && not (standIn a || standIn b)
      (D _, _) -> False
      _ -> True
    isRecord = case albumID a of D _ -> True; _ -> False
    masters x = Set.fromList (filter ("dm:" `TX.isPrefixOf`) (albumWikiKeys x))
    standIn x = any ((`Set.member` sit) . TX.toCaseFold) (albumTags x)
    -- shown as another album's row: a version of an album, a stand-in
    hidden k = rowOf ly k /= k
    -- words of two letters or more, and numbers ("Mind Maps 6" is not "Mind Maps 7")
    ws = filter (\w -> T.length w > 1 || TX.all Ch.isDigit w) . T.words . T.map (\c -> if Ch.isAlphaNum c then Ch.toLower c else ' ') . dropBrackets
    dropBrackets t = case T.breakOn "(" t of
      (x, r) | not (T.null r), not (T.null (T.strip x)) -> x
      _ -> t
    ta = ws (albumTitle a)
    aa = Set.fromList (ws (albumArtist a))
    scored =
      [ (s * 2 + (if shared then 1 else 0), b)
      | b <- M.elems am
      , fits b
      , not (hidden (albumID b))
      , nameOf ly (albumID b) /= nameOf ly (albumID a)
      , let tb = ws (albumTitle b)
      , let s = titleScore ta tb
      , let shared = not (Set.null (aa `Set.intersection` Set.fromList (ws (albumArtist b))))
      -- the same title is enough; a title inside a longer one ("The Gold" in "The Gold
      -- Collection", "Chopin", "Blues") or a similar one needs an artist word in common too
      -- (a record: the same master is enough, fits)
      , isRecord || s == 3 || (s >= 1 && shared)
      ]
    titleScore :: [Text] -> [Text] -> Int
    titleScore x y
      | null x || null y = 0
      | x == y = 3
      -- (a title inside a longer one counts only with a shared artist word: see scored)
      | x `L.isInfixOf` y || y `L.isInfixOf` x = 2
      | 5 * Set.size (sx `Set.intersection` sy) >= 3 * Set.size (sx `Set.union` sy) = 1
      | otherwise = 0
     where
      sx = Set.fromList x
      sy = Set.fromList y

  serveAlbumstream :: AlbumKey -> Maybe Text -> Maybe Text -> AppM ReqJ
  serveAlbumstream i mref mop = do
    now <- liftIO getZonedTime
    let dtl = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" now
        answer ok info = ReqJ{reqTime = dtl, reqInfo = info, reqResult = if ok then 1 else 0}
        logged act done = do
          wrote <- liftIO . try $ collectionFile >>= act
          case wrote :: Either SomeException () of
            Left _ -> pure $ answer False "Could not write the collection log (nothing changed)"
            Right () -> done
        name s = case s of
          T n -> "Tidal album " <> show n
          A n -> "Apple Music album " <> show n
          Q q -> "Qobuz album " <> q
          _ -> "stream"
    am <- asks albumsR >>= readIORef
    ly <- asks layerR >>= readIORef
    -- the album's streams (held or not)
    let mine = filter isStreamKey (versionsOfKey ly i)
        holder = case i of
          D _ -> True
          J _ -> True
          _ -> False
    case (M.lookup i am, mref >>= parseStreamRef, mop) of
      _ | not holder -> pure $ answer False "Only a Discogs release or a file album gets a stream linked here"
      _ | not (lyFormat ly) -> pure $ answer False notAlbumFormat
      (Nothing, _, _) -> pure $ answer False "Not in clutter"
      (Just a, Just s, Just "link")
        | s `elem` mine -> pure $ answer False ("Already linked: " <> name s)
        | otherwise ->
            -- one stream of a service per album, as before: the one it had leaves the album
            -- (several editions as versions come with slice 3). A stream that is a version
            -- of another album brings that album along: the same stream, the same album.
            let replaced = filter (sameKind s) mine
                ns = nameAny ly s
                along = case ns of
                  C _ | ns /= nameAny ly i -> ", together with the album it was a version of (" <> show (length (filter (/= s) (M.findWithDefault [] ns (lyVersions ly)))) <> " more versions; × in Versions takes one off)"
                  _ -> ""
             in logged (\fn -> forM_ replaced (\o -> appendSplit fn now o a) >> joinLines now ly False i s a fn) $ do
                  envReloadLog
                  pure $ answer True ("Linked " <> name s <> " to “" <> albumTitle a <> "”" <> (if null replaced then "" else ", in place of the one it had") <> along)
      (Just a, Just s, Just "unlink")
        | s `elem` mine ->
            logged (\fn -> appendSplit fn now s a) $ do
              envReloadLog
              pure $ answer True ("Took " <> name s <> " off “" <> albumTitle a <> "”")
        | otherwise -> pure $ answer False "That stream is not linked to this album"
      (Just _, Nothing, _) -> pure $ answer False "Not a Tidal, Apple Music or Qobuz album: paste T<id>, A<id>, Q<id> or a share link"
      _ -> pure $ answer False "op must be link or unlink"

  serveAlbumlink :: AlbumKey -> Maybe AlbumKey -> Maybe Text -> AppM ReqJ
  serveAlbumlink i mversion mop = do
    now <- liftIO getZonedTime
    let dtl = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" now
        answer ok info = ReqJ{reqTime = dtl, reqInfo = info, reqResult = if ok then 1 else 0}
        logged act done = do
          wrote <- liftIO . try $ collectionFile >>= act
          case wrote :: Either SomeException () of
            Left _ -> pure $ answer False "Could not write the collection log (nothing changed)"
            Right () -> done
    am <- asks albumsR >>= readIORef
    ly <- asks layerR >>= readIORef
    case (i, M.lookup i am, mversion, mop) of
      (J _, _, _, _) -> pure $ answer False "A file album cannot stand for another album"
      _ | not (lyFormat ly) -> pure $ answer False notAlbumFormat
      (_, Nothing, _, _) -> pure $ answer False "Not in clutter"
      (_, Just a, Just (J j), Just "link")
        | J j `elem` versionsOfKey ly i -> pure $ answer False "These files are a version of this album already"
        -- The file album leaves the album it was a version of alone, as before, where that
        -- album keeps a record; else (files with their Tidal album, say) it brings that
        -- album along, so that no stream is left behind on its own
        | M.member (J j) am ->
            let rest = filter (/= J j) (versionsOfKey ly (J j))
                alone = any isRecordKey rest
                along = if alone || null rest then "" else ", together with its " <> show (length rest) <> " other versions"
             in logged (joinLines now ly alone i (J j) a) $ do
                  envReloadLog
                  pure $ answer True ("Linked the files to “" <> albumTitle a <> "”" <> along)
        | otherwise -> pure $ answer False "No such file album"
      (_, Just a, Just (J j), Just "unlink")
        | J j `elem` versionsOfKey ly i ->
            logged (\fn -> appendSplit fn now (J j) a) $ do
              envReloadLog
              pure $ answer True ("Took the files off “" <> albumTitle a <> "”")
        | otherwise -> pure $ answer False "These files are not linked to this album"
      _ -> pure $ answer False "version must be a file album (J…), op link or unlink"

  -- The album layer by hand (docs/album-layer-plan.md › Slices, 3). Each answer carries
  -- the key of the album's row afterwards, the page the album is shown on.
  rowAnswer :: Bool -> Text -> AlbumKey -> AppM RowJ
  rowAnswer ok info k = do
    now <- liftIO getZonedTime
    pure RowJ{rjTime = toText (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" now), rjInfo = info, rjResult = if ok then 1 else 0, rjRow = k}

  -- write the lines, read the log again, answer with the row of k's album as it is now
  layerWrite :: (FilePath -> IO ()) -> Text -> AlbumKey -> AppM RowJ
  layerWrite act info k = do
    wrote <- liftIO . try $ collectionFile >>= act
    case wrote :: Either SomeException () of
      Left _ -> rowAnswer False "Could not write the collection log (nothing changed)" k
      Right () -> do
        envReloadLog
        ly <- asks layerR >>= readIORef
        rowAnswer True info (rowOf ly k)

  -- what a version is, for the answers
  versionName :: AlbumKey -> Text
  versionName k = case k of
    D _ -> "the record"
    T _ -> "the Tidal album"
    A _ -> "the Apple Music album"
    Q _ -> "the Qobuz album"
    J _ -> "the file album"
    C _ -> "the album"

  -- the versions of the album a key belongs to, as the page may act on them: for an album
  -- with an id its versions without the stand-in releases (shown as the row of an album
  -- none of whose versions clutter holds, but no version); otherwise the key alone
  membersOf :: Layer -> AlbumKey -> [AlbumKey]
  membersOf ly k = case nameOf ly k of
    C _ -> filter (`notElem` concat (M.elems (lyStandIns ly))) (versionsOfKey ly k)
    _ -> [k]

  isAlbumId :: AlbumKey -> Bool
  isAlbumId k = case k of C _ -> True; _ -> False

  -- other's album and aid's are one album: other's joins aid's (a version line, a new id
  -- and two version lines, or a merge into aid's: joinLines). Any two albums -- a file
  -- album to anything, a second edition of a stream, a second record; nothing is
  -- replaced (a version that does not belong is taken off with albumsplit).
  serveAlbumjoin :: AlbumKey -> Maybe AlbumKey -> AppM RowJ
  serveAlbumjoin i mo = do
    now <- liftIO getZonedTime
    am <- asks albumsR >>= readIORef
    ly <- asks layerR >>= readIORef
    let n k = length (membersOf ly k)
    case (M.lookup i am, mo, mo >>= (`M.lookup` am)) of
      _ | not (lyFormat ly) -> rowAnswer False notAlbumFormat i
      (Nothing, _, _) -> rowAnswer False "Not in clutter" i
      (_, Nothing, _) -> rowAnswer False "other is missing: the album to join this one" i
      (_, _, Nothing) -> rowAnswer False "The other album is not in clutter" i
      (Just a, Just o, Just b)
        | nameAny ly i == nameAny ly o -> rowAnswer False "These are one album already" i
        | otherwise ->
            layerWrite
              (joinLines now ly False i o a)
              ( (if albumTitle a == albumTitle b then "The two “" <> albumTitle a <> "”" else "“" <> albumTitle b <> "” and “" <> albumTitle a <> "”")
                  <> " are one album now, of " <> show (n i + n o) <> " versions"
              )
              i

  -- version v is taken off aid's album: its own album again (a split line). What was
  -- recorded against v's key goes with it (its plays); what was recorded against the
  -- album's id stays with the album.
  serveAlbumsplit :: AlbumKey -> Maybe AlbumKey -> AppM RowJ
  serveAlbumsplit i mv = do
    now <- liftIO getZonedTime
    am <- asks albumsR >>= readIORef
    ly <- asks layerR >>= readIORef
    let vs = membersOf ly i
        rest = filter (/= v') vs
        v' = fromMaybe i mv
        -- the Apple Music album a Tidal version of the album names (store lookup): split
        -- off, it would be that Tidal album's twin again, the same album (buildLayer)
        names t n = (albumAMusic =<< M.lookup t am) == Just (show n)
        twinOfHere = case v' of
          A n -> any (\k -> case k of T _ -> names k n; _ -> False) rest
          -- a Tidal album and the Apple album it names, alone: the Apple album would be
          -- its twin again
          T _ -> case rest of
            [A n] -> names v' n
            _ -> False
          _ -> False
    case (M.lookup i am, mv) of
      _ | not (lyFormat ly) -> rowAnswer False notAlbumFormat i
      (Nothing, _) -> rowAnswer False "Not in clutter" i
      (_, Nothing) -> rowAnswer False "version is missing: the version to take off" i
      (Just a, Just v)
        | v `notElem` vs -> rowAnswer False "That is not a version of this album" i
        | length vs < 2 -> rowAnswer False "An album of one version: nothing to take off" i
        | twinOfHere -> rowAnswer False "The Tidal album names this Apple Music album (store lookup): the two stay one album" i
        -- what was recorded against the album's id (keep mark, lead) stays with the rest:
        -- the rest must be an album clutter shows
        | not (any (`M.member` am) rest) -> rowAnswer False ("The rest of the album is not in clutter's library: take " <> versionName (fromMaybe v (viaNonEmpty head rest)) <> " off instead") i
        | otherwise ->
            layerWrite
              (\fn -> appendSplit fn now v (fromMaybe a (M.lookup v am)))
              ("Took " <> versionName v <> " off “" <> albumTitle a <> "”: an album of its own again")
              -- the page shows the album that remains
              (if v == i then fromMaybe i (find (`M.member` am) rest) else i)

  -- version v leads aid's album (a lead line): the album's title, cover and row are v's.
  -- Without a version: back to the rule (record, Tidal, files, Apple Music, Qobuz).
  serveAlbumlead :: AlbumKey -> Maybe AlbumKey -> AppM RowJ
  serveAlbumlead i mv = do
    now <- liftIO getZonedTime
    am <- asks albumsR >>= readIORef
    ly <- asks layerR >>= readIORef
    let nm = nameOf ly i
        vs = membersOf ly i
    case (M.lookup i am, mv) of
      _ | not (lyFormat ly) -> rowAnswer False notAlbumFormat i
      (Nothing, _) -> rowAnswer False "Not in clutter" i
      _ | not (isAlbumId nm) -> rowAnswer False "An album of one version: it leads itself" i
      (Just a, Just v)
        | v `notElem` vs -> rowAnswer False "That is not a version of this album" i
        | not (M.member v am) -> rowAnswer False "clutter does not hold that version, so it cannot be the album's row" i
        | rowOf ly i == v && Set.member nm (lyLeadSet ly) -> rowAnswer False "That version leads already" v
        | otherwise ->
            layerWrite
              (\fn -> appendLead fn now nm (Just v) a)
              ("Now " <> versionName v <> " leads “" <> albumTitle a <> "”")
              v
      (Just a, Nothing)
        | not (Set.member nm (lyLeadSet ly)) -> rowAnswer False "The album follows the rule already" i
        | otherwise ->
            layerWrite
              (\fn -> appendLead fn now nm Nothing a)
              "The lead follows the rule again (record, Tidal, files, Apple Music, Qobuz)"
              i

  -- any album row whose title and artist words begin with all the words asked for (two
  -- letters at least), not aid's own album; at most 20, by title
  serveAlbumfind :: AlbumKey -> Maybe Text -> AppM [CandV]
  serveAlbumfind i mq = do
    am <- asks albumsR >>= readIORef
    ly <- asks layerR >>= readIORef
    let norm = TX.words . TX.map (\c -> if Ch.isAlphaNum c then Ch.toLower c else ' ')
        ws = filter (\w -> TX.length w > 1 || TX.all Ch.isDigit w) (norm (fromMaybe "" mq))
        me = nameAny ly i
        hit b = let tw = norm (albumTitle b <> " " <> albumArtist b) in all (\w -> any (w `TX.isPrefixOf`) tw) ws
        candOf b = CandV{cvKey = albumID b, cvTitle = albumTitle b, cvArtist = albumArtist b, cvFormat = albumFormat b, cvReleased = albumReleased b, cvHas = ""}
    pure $
      if null ws
        then []
        else
          map candOf . take 20 . sortOn (\b -> (TX.toLower (albumTitle b), TX.toLower (albumArtist b))) $
            [b | b <- M.elems am, rowOf ly (albumID b) == albumID b, hit b, nameAny ly (albumID b) /= me]

  serveAlbumkeep :: AlbumKey -> Maybe Text -> AppM ReqJ
  serveAlbumkeep i mv = do
    now <- liftIO getZonedTime
    let dtl = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" now
        answer ok info = ReqJ{reqTime = dtl, reqInfo = info, reqResult = if ok then 1 else 0}
        mark = case mv of
          Just "yes" -> Just (Just True, "Kept")
          Just "no" -> Just (Just False, "Marked as not for me")
          Just "unset" -> Just (Nothing, "Mark taken off")
          _ -> Nothing
    am <- asks albumsR >>= readIORef
    ly <- asks layerR >>= readIORef
    case (M.lookup i am, mark) of
      (Nothing, _) -> pure $ answer False "Not in clutter"
      (_, Nothing) -> pure $ answer False "value must be yes, no or unset"
      (Just a, Just (m, said)) -> do
        -- against the album: its C id if it has one, else this key
        wrote <- liftIO . try $ collectionFile >>= \fn -> appendMark fn now (nameOf ly i) a m
        case wrote :: Either SomeException () of
          Left _ -> pure $ answer False "Could not write the collection log (nothing changed)"
          Right () -> do
            envReloadLog
            liftIO $ hPutStrLn stderr ("keep mark: " <> show i <> " " <> fromMaybe "?" (toString <$> mv))
            pure $ answer True (said <> ": “" <> albumTitle a <> "”")

  -- The inventory's matches that scripts/import-inventory-links.py left for review: not
  -- "sure" (the artist differs, or only the title agrees), or several albums fit (the vinyl
  -- and the CD). Read from its TSV (CLUTTER_INVENTORY, else
  -- cache/jellyfin-inventory/link-candidates.tsv) when asked for; a file album the log has
  -- decided (link, unlink, skip) or clutter does not hold is left out. Candidates: the
  -- inventory's choice -- a Tidal album a release has goes to that release -- then the
  -- title rule of the file album's page ("Same album as…"), five at most.
  serveReviewq :: AppM ReviewJ
  serveReviewq = do
    tsv <- liftIO $ fromMaybe "cache/jellyfin-inventory/link-candidates.tsv" . mfilter (not . null) <$> lookupEnv "CLUTTER_INVENTORY"
    got <- liftIO . try $ decodeUtf8 <$> readFileBS tsv
    case got :: Either SomeException Text of
      Left _ -> pure ReviewJ{rvRows = [], rvDone = 0, rvError = "No inventory: " <> toText tsv <> " (run scripts/jellyfin-inventory.py)"}
      Right body -> do
        ly <- asks layerR >>= readIORef
        -- decided: a line for it in the log, or a version of an album with others
        logged <- liftIO $ collectionFile >>= reviewedFiles
        let decided = logged <> Set.fromList [j | (J j, C _) <- M.toList (lyName ly)]
        -- the inventory has the ids of its day; a folder moved since has a new one
        moves <- liftIO $ collectionFile >>= readMoves
        sit <- liftIO standInFolderTags
        am <- asks albumsR >>= readIORef
        let rows = case map (TX.dropWhileEnd (== '\r')) (TX.lines body) of
              (h : ls) ->
                let cols = TX.splitOn "\t" h
                 in [M.fromList (zip cols (TX.splitOn "\t" l)) | l <- ls, not (TX.null (TX.strip l))]
              [] -> []
            col r c = M.findWithDefault "" c r
            unsure r = col r "grade" /= "sure" || not (TX.null (col r "flag"))
            open = filter unsure rows
            jid r = followMove moves (TX.toLower (col r "id"))
            done = length [() | r <- open, jid r `Set.member` decided]
            -- the row of the album this stream is a version of, if it is one
            holderOf s
              | M.member s am = Just (rowOf ly s)
              | otherwise = rowOf ly <$> (viaNonEmpty head . snd =<< find ((s `elem`) . snd) (M.toList (lyVersions ly)))
            proposal r = do
              k <- parseKey (col r "match")
              let k' = fromMaybe k (holderOf k)
              b <- M.lookup k' am
              pure CandV{cvKey = k', cvTitle = albumTitle b, cvArtist = albumArtist b, cvFormat = albumFormat b, cvReleased = albumReleased b, cvHas = ""}
            why r = case (col r "grade", col r "flag") of
              ("sure", f) -> f <> " fit — the inventory chose one"
              ("likely", "") -> "the artist differs a little"
              ("likely", f) -> "the artist differs a little; " <> f
              (_, "") -> "only the title agrees"
              (_, f) -> "only the title agrees; " <> f
            rank r = case col r "grade" of
              "likely" -> 0 :: Int
              "sure" -> 1
              _ -> 2
            row r = do
              let j = jid r
              guard (not (j `Set.member` decided))
              f <- M.lookup (J j) am
              -- (the review links with albumlink: no other file album as the target)
              let cands = take 5 (L.nubBy ((==) `on` cvKey) (maybeToList (proposal r) <> filter (not . isFileKey . cvKey) (candidatesFor sit ly am f)))
              pure
                ( (rank r, TX.toLower (albumArtist f))
                , ReviewRow
                    { rrKey = J j
                    , rrTitle = albumTitle f
                    , rrArtist = albumArtist f
                    , rrYear = col r "year"
                    , rrMins = col r "mins"
                    , rrStorage = fromMaybe "" (viaNonEmpty head (mapMaybe (TX.stripPrefix "storage.") (albumTags f)))
                    , rrFolder = fromMaybe "" (viaNonEmpty last (filter (not . TX.null) (TX.splitOn "/" (col r "path"))))
                    , rrUrl = albumURL f
                    , rrWhy = why r
                    , rrCands = cands
                    }
                )
        pure ReviewJ{rvRows = map snd (sortOn fst (mapMaybe row open)), rvDone = done, rvError = ""}

  serveAlbumskip :: AlbumKey -> AppM ReqJ
  serveAlbumskip i = do
    now <- liftIO getZonedTime
    let dtl = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" now
        answer ok info = ReqJ{reqTime = dtl, reqInfo = info, reqResult = if ok then 1 else 0}
    am <- asks albumsR >>= readIORef
    case (i, M.lookup i am) of
      (J _, Just f) -> do
        wrote <- liftIO . try $ collectionFile >>= \fn -> appendSkip fn now f
        case wrote :: Either SomeException () of
          Left _ -> pure $ answer False "Could not write the collection log (nothing changed)"
          Right () -> pure $ answer True ("Skipped “" <> albumTitle f <> "”")
      (J _, Nothing) -> pure $ answer False "No such file album"
      _ -> pure $ answer False "Only a file album (J…) is skipped"

  serveAlbumnotsame :: AlbumKey -> Maybe AlbumKey -> Maybe Text -> AppM ReqJ
  serveAlbumnotsame i mcand mop = do
    now <- liftIO getZonedTime
    let dtl = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" now
        answer ok info = ReqJ{reqTime = dtl, reqInfo = info, reqResult = if ok then 1 else 0}
        excluded = mop /= Just "undo"
    am <- asks albumsR >>= readIORef
    case (M.member i am, mcand >>= (`M.lookup` am)) of
      (False, _) -> pure $ answer False "Not in clutter"
      (_, Nothing) -> pure $ answer False "No such suggestion (cand=…)"
      (True, Just c) -> do
        wrote <- liftIO . try $ collectionFile >>= \fn -> appendNotSame fn now i c excluded
        case wrote :: Either SomeException () of
          Left _ -> pure $ answer False "Could not write the collection log (nothing changed)"
          Right () -> pure $ answer True ((if excluded then "Not suggested again: “" else "Suggested again: “") <> albumTitle c <> "”")

  -- the keep mark for many albums: those clutter holds and whose mark is not that already
  serveAlbumskeep :: Maybe Text -> [AlbumKey] -> AppM ReqJ
  serveAlbumskeep mv keys = do
    now <- liftIO getZonedTime
    let dtl = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" now
        answer ok info = ReqJ{reqTime = dtl, reqInfo = info, reqResult = if ok then 1 else 0}
        mark = case mv of
          Just "yes" -> Just (Just True, "Kept ")
          Just "no" -> Just (Just False, "Marked as not for me: ")
          Just "unset" -> Just (Nothing, "Mark taken off ")
          _ -> Nothing
    am <- asks albumsR >>= readIORef
    ly <- asks layerR >>= readIORef
    case mark of
      Nothing -> pure $ answer False "value must be yes, no or unset"
      Just (m, said) -> do
        -- one line per album (two versions of one album selected: one mark)
        let byAlbum = M.fromListWith (\_ earlier -> earlier) [(nameOf ly k, a) | k <- ordNub keys, Just a <- [M.lookup k am]]
            todo = M.toList (M.filterWithKey (\nk _ -> M.lookup nk (lyKeep ly) /= m) byAlbum)
            n = length todo
            counted c = show c <> (if c == 1 then " album" else " albums")
            -- selected albums whose mark was that already, or keys clutter does not hold
            same = length (ordNub keys) - length [() | k <- ordNub keys, M.member k am] + M.size byAlbum - n
            rest = if same > 0 then " (" <> show same <> " as before or not in clutter)" else ""
        if null todo
          then pure $ answer True ("Nothing to change" <> rest)
          else do
            wrote <- liftIO . try $ collectionFile >>= \fn -> forM_ todo (\(k, a) -> appendMark fn now k a m)
            case wrote :: Either SomeException () of
              -- lines written before the failure are in the log and count at the next start
              Left _ -> pure $ answer False "Could not write the collection log"
              Right () -> do
                envReloadLog
                liftIO $ hPutStrLn stderr ("keep mark: " <> show n <> " albums " <> fromMaybe "?" (toString <$> mv))
                pure $ answer True (said <> counted n <> rest)

  serveParamsq :: AppM ParamsJ
  serveParamsq = do
    liftIO $ print ("-------serveParamsq " :: Text)
    now <- liftIO getZonedTime -- `debugId`
    let dtl = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" now
    ms <- envUpdateMenuParams
    let sj =
          ParamsJ
            { timeStamp = dtl
            , params = ms
            }
    pure sj

  serveReq :: Maybe Text -> AppM ReqJ
  serveReq r = do
    liftIO $ print ("-------serveReq " :: Text, r)
    now <- liftIO getZonedTime
    let dtl = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" now
    let sj =
          ReqJ
            { reqTime = dtl
            , reqInfo = fromMaybe "" r
            , reqResult = 1
            }
    case r of
      Just "update" -> envUpdate Nothing Nothing 20 >> pure sj
      -- default is to update last 20 release records
      -- Tidal: the favourites and the Save for later list, without a restart. update-json
      -- --tidal refreshes the token and brings the album store up to date; its output is
      -- dropped unread. Then the store is taken in again.
      Just "tidal" -> do
        ran <- liftIO . try $ readProcessWithExitCode "./update-json" ["--tidal"] ""
        let failed = case ran :: Either SomeException (ExitCode, String, String) of
              Right (ExitSuccess, _, _) -> Nothing
              Right (ExitFailure c, _, _) -> Just ("update-json --tidal failed (" <> show c <> ")")
              Left _ -> Just "update-json could not be run"
        new <- envTidalReload
        liftIO $ hPutStrLn stderr ("update from Tidal: " <> show new <> " new albums" <> maybe "" (\e -> "; " <> toString e) failed)
        pure $ case failed of
          Nothing -> sj{reqInfo = show new <> (if new == 1 then " new album" else " new albums") <> " from Tidal"}
          Just e -> sj{reqInfo = e, reqResult = 0}
      -- the file server: scripts/update-jellyfin-albums.py asks Jellyfin for its albums and
      -- writes clutter's store; then the store is taken in again, without a restart
      Just "jellyfin" -> jellyfinUpdate []
      -- every album, not only the new ones: minutes (Jellyfin takes about 40 ms per album)
      Just "jellyfinfull" -> jellyfinUpdate ["--full"]
      _ -> liftIO (print r) >> pure sj

  -- the file server: scripts/update-jellyfin-albums.py asks Jellyfin for its albums -- the
  -- new ones, or with --full all of them -- and writes clutter's store; then the store is
  -- taken in again, without a restart
  jellyfinUpdate :: [String] -> AppM ReqJ
  jellyfinUpdate flags = do
    now <- liftIO getZonedTime
    let sj = ReqJ{reqTime = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" now, reqInfo = "", reqResult = 1}
    ran <- liftIO . try $ readProcessWithExitCode "python3" ("scripts/update-jellyfin-albums.py" : flags) ""
    let lastLine = viaNonEmpty last . filter (not . TX.null) . TX.lines . toText
    case ran :: Either SomeException (ExitCode, String, String) of
      Left _ -> pure sj{reqInfo = "scripts/update-jellyfin-albums.py could not be run", reqResult = 0}
      -- the script's last line says why (Jellyfin not reachable, no key)
      Right (ExitFailure _, _, err) -> do
        let e = "Update from the server failed: " <> fromMaybe "no reason given" (lastLine err)
        liftIO $ hPutStrLn stderr (toString e)
        pure sj{reqInfo = e, reqResult = 0}
      Right (ExitSuccess, _, err) -> do
        (new, gone, moved, dangling) <- envJellyfinReload
        let n c sg pl = show c <> " " <> (if c == 1 then sg else pl)
            -- the quick run saw by Jellyfin's album count that more than additions changed
            due = maybe False ("full update" `TX.isInfixOf`) (lastLine err)
            info =
              -- short: the top bar shows one line
              "Server: " <> show new <> " new"
                <> (if gone > 0 then ", " <> show gone <> " gone" else "")
                <> (if moved > 0 then ", " <> show moved <> " moved (links kept)" else "")
                <> (if dangling > 0 then ", " <> n dangling "link" "links" <> " without files" else "")
                <> (if due then " — albums left or moved: a full update is due" else "")
        liftIO $ hPutStrLn stderr (toString info)
        pure sj{reqInfo = info}

  serveNodeplay :: Int -> Maybe Text -> AppM ReqJ
  serveNodeplay tid mm = do
    liftIO $ print ("-------serveNodeplay " :: Text, tid, mm)
    now <- liftIO getZonedTime
    let dtl = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" now
    base <- liftIO bluosBase
    (ok, info) <- liftIO $ bluosAdd base tid (fromMaybe "replace" mm)
    liftIO $ hPutStrLn stderr ("nodeplay " <> show tid <> ": " <> toString info)
    pure ReqJ{reqTime = dtl, reqInfo = info, reqResult = if ok then 1 else 0}

  -- the files of an album on the Node: the file album's own title and artist are what the
  -- player has read from the same tags
  serveNodeplayfiles :: AlbumKey -> Maybe Text -> Maybe AlbumKey -> AppM ReqJ
  serveNodeplayfiles i mm mversion = do
    now <- liftIO getZonedTime
    let dtl = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" now
        answer ok info = ReqJ{reqTime = dtl, reqInfo = info, reqResult = if ok then 1 else 0}
    am <- asks albumsR >>= readIORef
    let files = case (i, M.lookup i am) of
          (J _, Just f) -> Just f
          (_, Just a) -> case mversion of
            Just (J j) | j `elem` albumFiles a -> M.lookup (J j) am
            Just _ -> Nothing
            Nothing -> viaNonEmpty head (albumFiles a) >>= \j -> M.lookup (J j) am
          _ -> Nothing
    case files of
      Nothing -> pure $ answer False "No files of this album are known to clutter"
      Just f -> do
        base <- liftIO bluosBase
        let mode = fromMaybe "replace" mm
        -- by folder where the player has the album's folder in one of its shares (exact);
        -- else by album and artist tag
        mfolder <- liftIO $ maybe (pure Nothing) nodeFolder (fileKey f)
        (ok, info) <- liftIO $ case mfolder of
          Just folder -> bluosAddFolder base folder (albumTitle f) mode
          Nothing -> bluosAddLocal base (albumTitle f) (albumArtist f) mode
        liftIO $ hPutStrLn stderr ("nodeplayfiles " <> show (albumID f) <> maybe " (by tags)" (\p -> " (" <> toString p <> ")") mfolder <> ": " <> toString info)
        pure $ answer ok info

  serveNodectl :: Text -> AppM ReqJ
  serveNodectl cmd = do
    now <- liftIO getZonedTime
    let dtl = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" now
    base <- liftIO bluosBase
    (ok, info) <- liftIO $ bluosCtl base cmd
    liftIO $ hPutStrLn stderr ("nodectl " <> toString cmd <> ": " <> toString info)
    pure ReqJ{reqTime = dtl, reqInfo = info, reqResult = if ok then 1 else 0}

  serveNodeinfo :: AppM ReqJ
  serveNodeinfo = do
    now <- liftIO getZonedTime
    let dtl = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" now
    base <- liftIO bluosBase
    r <- liftIO $ bluosGet base "/SyncStatus"
    let (ok, info) = case r of
          Left err -> (False, err)
          -- <SyncStatus … name="lblue" model="N130" modelName="NODE" …>
          Right b -> case T.stripPrefix " name=\"" . snd $ T.breakOn " name=\"" b of
            Just rest | T.takeWhile (/= '"') rest /= "" -> (True, T.takeWhile (/= '"') rest)
            _ -> (False, "Node: no name in /SyncStatus")
    liftIO $ hPutStrLn stderr ("nodeinfo: " <> toString info)
    pure ReqJ{reqTime = dtl, reqInfo = info, reqResult = if ok then 1 else 0}

  serveAlbump :: AlbumKey -> AppM AlbumJ
  serveAlbump i = do
    liftIO $ print ("-------serveAlbump " :: Text, i)
    -- a Tidal or Apple album is played as it is: refreshing it through
    -- envUpdateAlbum would re-read Discogs folders and lists
    known <- asks albumsR >>= readIORef
    ma <- case M.lookup i known of
      Just a | not (isDiscogs (albumID a)) -> pure (Just a)
      _ -> envUpdateAlbum i
    maybe (print ("no Album" :: Text)) updateAlbumsPlayed ma
    -- the album again, now with this play in its dates
    ma' <- case ma of
      Just a -> Just . fromMaybe a . M.lookup i <$> (asks albumsR >>= readIORef)
      Nothing -> pure Nothing
    let aj = case ma' of
          Just a -> AlbumJ{aid = i, album = Just a}
          _ -> AlbumJ{aid = i, album = Nothing}
    pure aj

  serveApp :: AppM RawHtml
  serveApp = do
    now <- liftIO getZonedTime -- `debugId`
    liftIO $ print $ ("-------serveApp " :: Text) <> show now
    html <- renderApp "Clutter Prototype"
    pure . RawHtml $ L.renderBS html

  serveAlbum :: AlbumKey -> AppM RawHtml
  serveAlbum a = do
    liftIO $ print ("-------serveAlbum " :: Text, a)
    now <- liftIO getZonedTime -- `debugId`
    ma <- envUpdateAlbum a
    html <- renderAlbumView ma now
    pure . RawHtml $ L.renderBS html

  serveAlbums ::
    Text ->
    Maybe Text ->
    Maybe Text ->
    [Text] ->
    AppM RawHtml
  serveAlbums ln msb mso fs = do
    aids <- decodeListQuery ln msb mso fs
    html <- renderAlbumsView ln fs aids
    pure . RawHtml $ L.renderBS html

  serveDiscogs :: Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> AppM RawHtml
  serveDiscogs token username nreleases release = do
    liftIO $ print ("------Updating from Discogs" :: Text, token, username, nreleases, release)
    envUpdate token username (fromMaybe 0 nreleases)
    let ln :: Text; ln = "Discogs"
    gl <- asks getList
    aids <- gl ln
    html <- renderAlbumsView ln [] aids
    pure . RawHtml $ L.renderBS html

  serveTidal :: Maybe Int -> AppM RawHtml
  serveTidal nalbums = do
    liftIO $ print ("------Connecting to Tidal" :: Text)
    _ <- envTidalConnect (fromMaybe 0 nalbums)
    let ln = "Tidal"
    gl <- asks getList
    aids <- gl ln
    html <- renderAlbumsView ln [] aids
    pure . RawHtml $ L.renderBS html

-- the player's address: CLUTTER_BLUOS, else the Node's mDNS name
bluosBase :: IO String
bluosBase = fromMaybe "http://lblue.local:11000" <$> lookupEnv "CLUTTER_BLUOS"

-- BluOS: queue a Tidal album by its Tidal album id and play it. The parameters are
-- those of the "Play now" entry in the player's own /Services menu (Tidal-Album);
-- tested live on the Node 2026-11-21, see ~/Notes/Claude/2026-11-21-bluos-api-for-clutter.md
-- Returns (ok, message); never throws.
bluosAdd :: String -> Int -> Text -> IO (Bool, Text)
bluosAdd base tid = bluosAddWhat base ("service=Tidal&albumid=" <> show tid)

-- BluOS: queue an album of the player's own library (the network shares it has indexed)
-- and play it. The player knows such an album by its album and artist tags, not by an id
-- ("Play now" in /Services, LocalMusic-Album); tested live 2026-11-25. An album it does
-- not have is answered with <error>…Nothing here can be played…</error>.
bluosAddLocal :: String -> Text -> Text -> Text -> IO (Bool, Text)
bluosAddLocal base title artist mode = do
  (ok, info) <- bluosAddWhat base (toString ("service=LocalMusic&album=" <> urlEnc title <> "&artist=" <> urlEnc artist)) mode
  pure $
    if not ok && "Nothing here can be played" `T.isInfixOf` info
      then (False, "Not in the player's library: “" <> title <> "” by " <> artist <> " (its folder is not shared with the player, or it is tagged differently there)")
      else (ok, info)

-- BluOS: queue a folder of the player's library, with everything below it (the discs of a
-- box) -- "Play now" of the Folder context in /Services. The path is the share as the
-- player names it plus the folder below, "//host/share/…/Artist/Album".
bluosAddFolder :: String -> Text -> Text -> Text -> IO (Bool, Text)
bluosAddFolder base folder title mode = do
  (ok, info) <- bluosAddWhat base (toString ("service=LocalMusic&path=" <> urlEnc folder)) mode
  pure $
    if not ok && "Nothing here can be played" `TX.isInfixOf` info
      then (False, "The player finds nothing to play in the folder of “" <> title <> "” (still indexing, or files it cannot play)")
      else (ok, info)

-- percent-encoding of the UTF-8 bytes; letters, digits and -._~ stay
urlEnc :: Text -> Text
urlEnc = toText . concatMap enc . BS.unpack . encodeUtf8
 where
  enc w
    | w < 128 && (Ch.isAlphaNum c || c `elem` ("-._~" :: String)) = [c]
    | otherwise = '%' : (if w < 16 then "0" else "") <> map Ch.toUpper (showHex w "")
   where
    c = Ch.chr (fromIntegral w)

-- the file album whose files stand for an album: itself, or its first file version
fileKey :: Album -> Maybe Text
fileKey a = case albumID a of
  J j -> Just j
  _ -> viaNonEmpty head (albumFiles a)

-- The shares the player has indexed, as it names them ("//umini/media/music/Music"): the
-- top of its folder browse. Asked for at most every two minutes; an unreachable player
-- has none.
nodeShares :: IO [Text]
nodeShares = do
  now <- getCurrentTime
  cached <- readIORef nodeSharesCache
  case cached of
    Just (t, ss) | diffUTCTime now t < 120 -> pure ss
    _ -> do
      base <- bluosBase
      r <- bluosGet base "/library/v1/Folders?service=LocalMusic"
      let ss = case r of
            Right b -> map unescape (between b)
            Left _ -> []
      writeIORef nodeSharesCache (Just (now, ss))
      pure ss
 where
  between b = case TX.breakOn "<folder>" b of
    (_, rest) | not (TX.null rest) -> let (x, more) = TX.breakOn "</folder>" (TX.drop 8 rest) in x : between more
    _ -> []
  unescape = TX.replace "&amp;" "&" . TX.replace "&apos;" "'" . TX.replace "&quot;" "\"" . TX.replace "&gt;" ">" . TX.replace "&lt;" "<"

{-# NOINLINE nodeSharesCache #-}
nodeSharesCache :: IORef (Maybe (UTCTime, [Text]))
nodeSharesCache = unsafePerformIO (newIORef Nothing)

-- The folder of a file album as the player names it, if one of its shares holds it.
-- Jellyfin and the player see the same folders under different roots
-- ("/media/music/Music/…" and "//umini/media/music/Music/…"), so the share is laid over
-- the Jellyfin path where the end of the share's name occurs in it -- at least two folder
-- names, or everything but the host -- and the rest of the path is the folder below.
nodeFolder :: Text -> IO (Maybe Text)
nodeFolder j = do
  paths <- jellyfinPaths
  case M.lookup j paths of
    Nothing -> pure Nothing
    Just p -> (\ss -> folderOn ss p) <$> nodeShares

folderOn :: [Text] -> Text -> Maybe Text
folderOn shares path = viaNonEmpty head . map snd . sortOn (Down . fst) $ mapMaybe try1 shares
 where
  comps = filter (not . TX.null) . TX.splitOn "/"
  p = comps path
  try1 share =
    let s = comps share
        fits k =
          let suf = drop (length s - k) s
           in viaNonEmpty head [rest | i <- [0 .. length p - k], take k (drop i p) == suf, let rest = drop (i + k) p, not (null rest)]
     in viaNonEmpty head
          [ (k, TX.dropWhileEnd (== '/') share <> "/" <> TX.intercalate "/" rest)
          | k <- reverse [1 .. length s]
          , k >= 2 || k == length s - 1
          , Just rest <- [fits k]
          ]

-- the album to add ("service=…&albumid=…", "service=…&album=…&artist=…" or "service=…&path=…") and the mode
bluosAddWhat :: String -> String -> Text -> IO (Bool, Text)
bluosAddWhat base what mode = do
  let ps :: String
      ps = case mode of
        "now" -> "&playnow=1&clear=0&shuffle=0&where=nextAlbum" -- after the current album
        "next" -> "&playnow=-1&where=nextAlbum" -- queue, don't play
        "last" -> "&playnow=-1&where=last"
        _ -> "&playnow=1&clear=1&where=nextAlbum" -- replace the queue
  r <- bluosGet base ("/Add?" <> what <> ps)
  pure $ case r of
    Left err -> (False, err)
    Right b ->
      -- reply is <playlist length="6" id="362" …/> or <error …><message>…</message></error>
      let len = maybe "?" (T.takeWhile (/= '"')) . T.stripPrefix "length=\"" . snd $ T.breakOn "length=\"" b
       in if "<error" `T.isInfixOf` b || not ("<playlist" `T.isInfixOf` b)
            then (False, "Node refused: " <> b)
            else (True, "Node: " <> (if mode `elem` ["next", "last"] then "queued, " else "playing, ") <> len <> " tracks in queue")

-- BluOS transport: toggle (pause/resume), pause, play, stop.
-- The player answers <state>pause</state> (or play/stop/stream).
bluosCtl :: String -> Text -> IO (Bool, Text)
bluosCtl base cmd = case cmdPath of
  Nothing -> pure (False, "Node: unknown command " <> cmd)
  Just p -> do
    r <- bluosGet base p
    pure $ case r of
      Left err -> (False, err)
      Right b ->
        let st = T.takeWhile (/= '<') . snd . T.breakOn ">" . snd $ T.breakOn "<state" b
         in if "<error" `T.isInfixOf` b
              then (False, "Node refused: " <> b)
              -- /Skip answers with the new track's id, not a state
              else (True, "Node: " <> (if st == "" then (if cmd == "skip" then "next track" else b) else fromMaybe st (T.stripPrefix ">" st)))
 where
  cmdPath :: Maybe String
  cmdPath = case cmd of
    "toggle" -> Just "/Pause?toggle=1"
    "pause" -> Just "/Pause"
    "play" -> Just "/Play"
    "stop" -> Just "/Stop"
    "skip" -> Just "/Skip" -- the next track in the queue
    _ -> Nothing

-- one GET to the player; Left is a short, human-readable failure
bluosGet :: String -> String -> IO (Either Text Text)
bluosGet base path = do
  r <- try $ do
    mgr <- HC.newManager HC.defaultManagerSettings{HC.managerResponseTimeout = HC.responseTimeoutMicro 15000000}
    req <- HC.parseRequest (base <> path)
    HC.responseBody <$> HC.httpLbs req mgr
  pure $ case r of
    Left (HC.HttpExceptionRequest _ c) -> Left ("Node not reachable at " <> toText base <> ": " <> show c)
    Left e -> Left ("Node: bad request " <> show e)
    Right body -> Right (decodeUtf8 body)

isDiscogs :: AlbumKey -> Bool
isDiscogs k = case k of
  D _ -> True
  _ -> False

updateAlbumsPlayed :: Album -> AppM ()
updateAlbumsPlayed a = do
  now <- liftIO getZonedTime
  let key = albumID a
  --
  -- clutter's own play log comes first, for every kind of album
  ldir <- liftIO listenedDir
  let pe = playEvent now a
  logged <- liftIO . try $ appendPlay ldir pe
  liftIO . hPutStrLn stderr $ case logged of
    Right fn -> "listened: " <> show key <> " -> " <> fn
    Left (err :: SomeException) -> "ERROR listened: could not log " <> show key <> " in " <> ldir <> ": " <> show err
  -- and show up in the Listened lists and play counts without a restart
  when (isRight logged) $ do
    asks playsR >>= \r -> modifyIORef' r (<> [pe])
    envRefreshPlays
  --
  -- add a Note in Obsidian
  -- assumes that the Obsidian Vault is linked to `./AlbumsPlayed`
  let ts = renderAlbumText a now

  let fn = fromMaybe "!!error!!" $ viaNonEmpty head ts
  let pathName = "AlbumsPlayed/" <> fn <> ".md"
  putTextLn ("-----updating albums played: " <> pathName)
  liftIO $ TIO.writeFile (T.unpack pathName) (T.unlines $ drop 1 ts)
  --
  -- enter a journal entry in Day One
  -- enable Day One CLI w/ `sudo bash /Applications/Day\ One.app/Contents/Resources/install_cli.sh`
  -- make sure Journal named "Albums Played" does already exist
  --  let ns = renderAlbumDayOne a
  let js = map T.unpack $ renderAlbumJournal a now
  putTextLn "-----add entry into Day One Journal Albums Played, file in AlbumsPlayed"
  print js
  let iurl = fromMaybe "!!error!!" $ viaNonEmpty head js
  -- The Day One CLI (symlinked from /usr/local/bin into the app bundle since
  -- 2026-09-18) runs App-Sandboxed and can only read its own containers, so
  -- the cover must be staged inside the Day One app-group container, not /tmp.
  home <- fromMaybe "/tmp" <$> lookupEnv "HOME"
  let itmp = home <> "/Library/Group Containers/5U8NS4GX82.dayoneapp2/__ac.jpg"
  -- -f: an HTTP error body (Discogs "You are making requests too quickly")
  -- must not be left behind masquerading as a JPEG.
  -- --retry: opening an album fires a burst of Discogs API calls just before
  -- this, and i.discogs.com then answers 429 (seen 2026-11-19); curl treats
  -- 429 as transient, backs off (honouring Retry-After) and tries again.
  exitCode <- liftIO $ rawSystem "curl" ["-fsSL", "--retry", "5", "--retry-max-time", "60", iurl, "-o", itmp]
  unless (exitCode == ExitSuccess) $
    putTextLn $
      "ERROR getting cover img: curl failed with exit code: " <> show exitCode
  --
  -- only a Discogs release has a Discogs id: a Tidal or Apple id sent to
  -- /wants/ or /releases/ would 404, or hit an unrelated release
  dc <- envGetDiscogs
  case (key, dc) of
    (D rid, DiscogsSession ttok tu) -> updateDiscogsPlayed ttok tu rid a
    (D _, _) -> putTextLn "-----no Discogs session, Discogs not updated"
    _ -> liftIO . hPutStrLn stderr $ "listened: " <> show key <> " is not a Discogs release, no Discogs calls"

  putTextLn ""
  -- only attach the cover if the download actually succeeded
  let attachArgs = if exitCode == ExitSuccess then ["-a", itmp] else []
  let args = ["-j", "Albums Played"] <> attachArgs <> ["--", "new"] <> drop 1 js
  exitCode' <- liftIO $ rawSystem "dayone" args
  unless (exitCode' == ExitSuccess) $
    putTextLn $
      "ERROR adding to DayOne.app: failed with exit code: " <> show exitCode'
  print args

  pure ()

-- Discogs-side bookkeeping for a play of a Discogs release:
-- mark the collection field 7 "Play Count"; the Want list stamp is
-- replaced by the play log and only written with CLUTTER_DISCOGS_WANTS=1
updateDiscogsPlayed :: Text -> Text -> Int -> Album -> AppM ()
updateDiscogsPlayed ttok tu rid a = do
  now <- liftIO getZonedTime
  let taid = show rid :: Text
  let tinst = show $ albumInst a
  let auth = "Authorization: Discogs token=" <> unpack ttok
      wantsURL = "https://api.discogs.com/users/" <> unpack tu <> "/wants/" <> unpack taid

  wantsFlag <- lookupEnv "CLUTTER_DISCOGS_WANTS"
  when (wantsFlag == Just "1") $ do
    putTextLn "-----add to Discogs Want List (CLUTTER_DISCOGS_WANTS=1)"
    -- see if this release is on Want list, and get current notes
    res <-
      ( eitherDecode . TLE.encodeUtf8 . TL.pack
          <$> liftIO
            ( readProcess
                "curl"
                [ "-X"
                , "PUT"
                , "-H"
                , auth
                , "-H"
                , "Content-Type: application/json; charset=utf-8"
                , wantsURL
                ]
                ""
            )
      ) ::
        AppM (Either String WantsJ)
    case res of
      Left err -> putTextLn $ "Error decoding Want List: " <> show err
      Right wants -> print wants -- pure ()
    let tno = case res of
          Left _ -> ""
          Right wants -> wnotes wants

    let td = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" now
        tn = case tno of
          "" -> td
          _ -> td <> "\n" <> tno
    exitCode3 <-
      liftIO $
        rawSystem
          "curl"
          [ "-X"
          , "POST"
          , "-H"
          , auth
          , "-H"
          , "Content-Type: application/json; charset=utf-8"
          , "--data"
          , jsonBody ["notes" .= tn]
          , wantsURL
          ]
    unless (exitCode3 == ExitSuccess) $
      putTextLn $
        "ERROR adding date to Discogs Want List: failed with exit code: " <> show exitCode3
  --
  -- mark the physical copy as played: field 7 "Play Count" (a dropdown 1..6, Many) is set
  -- to 1 when empty and otherwise left alone -- clutter's play log does the counting
  let tfield = "7"
      tpls = "1" :: Text
  if albumInst a == 0
    then putTextLn "-----not in the Discogs collection, Play Count not marked"
    else if albumPlays a > 0
    then putTextLn "-----Play Count already marked on Discogs"
    else do
      exitCode4 <-
        liftIO $
          rawSystem
            "curl"
            [ "-X"
            , "POST"
            , "-H"
            , auth
            , "-H"
            , "Content-Type: application/json; charset=utf-8"
            , "--data"
            , jsonBody ["value" .= tpls]
            , "https://api.discogs.com/users/" <> unpack tu <> "/collection/folders/0/releases/" <> unpack taid <> "/instances/" <> unpack tinst <> "/fields/" <> unpack tfield
            ]
      unless (exitCode4 == ExitSuccess) $
        putTextLn $
          "ERROR marking Discogs Play Count: failed with exit code: " <> show exitCode4
 where
  jsonBody = TL.unpack . TLE.decodeUtf8 . encode . object

-- init env from files (AppM not yet available) and run app
startApp :: Int -> Bool -> IO ()
startApp p c = do
  sd <- resolveStaticDir
  ldir <- listenedDir
  hPutStrLn stderr $ "logging plays to " <> ldir
  env <- envInit c
  run p (appWith sd env)

---
--- the static assets (index.js, style.css, icons) live next to the sources, not in
--- the installed package, and were looked up relative to the process working
--- directory -- so a restart from anywhere else served a page with no script and no
--- stylesheet, with nothing in the log to say why. Resolve the directory once at
--- startup, make it absolute so a later chdir cannot break it, allow an override,
--- and complain loudly if the bundle is not where we think it is.
---
resolveStaticDir :: IO FilePath
resolveStaticDir = do
  sd <- makeAbsolute . fromMaybe "static" =<< lookupEnv "CLUTTER_STATIC"
  haveBundle <- doesFileExist (sd </> "index.js")
  --- stderr, not stdout: stdout is block-buffered once redirected to a log file,
  --- so a warning printed there can sit unflushed for the life of the process.
  if haveBundle
    then hPutStrLn stderr $ "serving static assets from " <> sd
    else
      hPutStrLn stderr $
        "WARNING: no index.js under "
          <> sd
          <> " -- the web UI will not load. Start clutter from the clutter"
          <> " source directory, or point CLUTTER_STATIC at its static/ directory."
  pure sd
