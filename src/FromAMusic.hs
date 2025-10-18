{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module FromAMusic (
  readAMusicReleases,
) where

import Relude

-- import Relude.File
import qualified Relude.Unsafe as Unsafe

import Control.Exception (catch)
import Control.Monad.Loops (unfoldrM)
import Data.Hashable (hash)
import qualified Data.Text as T

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.!=), (.:), (.:?))

-- import GHC.Generics

-- import Data.Proxy
import Servant

-- import Servant.API
import Servant.Client

import Types (AMusicInfo (..), Release (..), TagFolder (..))

readInt :: String -> Int -- crash if not an integer
readInt = Unsafe.fromJust . readMaybe

data WAMusic = WAMusic
  { data_ :: [WAMData]
  , meta_ :: WAMMeta
  , next_ :: !Text
  }
  deriving (Show, Generic)
instance FromJSON WAMusic where
  parseJSON = withObject "wamusic" $ \o -> do
    d_ <- o .: "data"
    m_ <- o .:? "meta" .!= WAMMeta 150 -- !!! recently-added does not have a meta field, and will return at most 150 entries
    n_ <- o .:? "next" .!= ""
    pure $ WAMusic d_ m_ n_
data WAMData = WAMData
  { id :: !Text
  , type__ :: !Text
  , attributes :: WAMAttr
  , relationships :: WAMRel
  }
  deriving (Show, Generic)
instance FromJSON WAMData where
  parseJSON = withObject "wamdata" $ \o -> do
    id_ <- o .: "id"
    type_ <- o .: "type"
    attr_ <- o .: "attributes"
    rel_ <- o .: "relationships"
    pure $ WAMData id_ type_ attr_ rel_
newtype WAMMeta = WAMMeta {total :: Int}
  deriving (Show, Generic)
instance FromJSON WAMMeta
data WAMAttr = WAMAttr
  { trackCount :: Int
  , genreNames :: [Text]
  , releaseDate :: !Text
  , name :: !Text
  , artistName :: !Text
  , artwork :: WAMArt
  , dateAdded :: !Text
  }
  deriving (Show, Generic)
instance FromJSON WAMAttr where
  parseJSON = withObject "attr" $ \o -> do
    trackCount_ <- o .:? "trackCount" .!= 0
    genreNames_ <- o .:? "genreNames" .!= []
    releaseDate_ <- o .:? "releaseDate" .!= ""
    name_ <- o .: "name"
    artistName_ <- o .:? "artistName" .!= ""
    artwork_ <- o .:? "artwork" .!= WAMArt 0 0 ""
    dateAdded_ <- o .: "dateAdded"
    pure $ WAMAttr trackCount_ genreNames_ releaseDate_ name_ artistName_ artwork_ dateAdded_
data WAMArt = WAMArt
  { width :: Int
  , height :: Int
  , url :: !Text
  }
  deriving (Show, Generic)
instance FromJSON WAMArt where
  parseJSON = withObject "artwork" $ \o -> do
    w_ <- o .:? "width" .!= 0
    h_ <- o .:? "height" .!= 0
    url_ <- o .:? "url" .!= ""
    pure $ WAMArt w_ h_ url_
newtype WAMRel = WAMRel {catalog :: WAMCat}
  deriving (Show, Generic)
instance FromJSON WAMRel
newtype WAMCat = WAMCat {catdata :: [WAMCatData]}
  deriving (Show, Generic)
instance FromJSON WAMCat where
  parseJSON = withObject "wamcat" $ \o -> do
    d_ <- o .: "data"
    pure $ WAMCat d_
data WAMCatData = WAMCatData
  { id :: !Text
  , attributes :: WAMCatDataAttr
  }
  deriving (Show, Generic)
instance FromJSON WAMCatData
newtype WAMCatDataAttr = WAMCatDataAttr {trackCount :: Int}
  deriving (Show, Generic)

instance FromJSON WAMCatDataAttr where
  parseJSON = withObject "wamcatdataattr" $ \o -> do
    tc_ <- o .:? "trackCount" .!= 0
    pure $ WAMCatDataAttr tc_

type UserAgent = Text
type AMusicInclude = Text
type AMusicDeveloperToken = Text
type AMusicUserToken = Text
type AMusicLimit = Int
type AMusicOffset = Int
type AMusicAPI =
  -- GET all Apple Music library albums, in alphabetic order; max 100 at a time
  -- https://api.music.apple.com/v1/me/library/albums?include=catalog&limit=limit&offset=offset
  "me"
    :> "library"
    :> "albums"
    :> QueryParam "include" AMusicInclude
    :> QueryParam "limit" AMusicLimit
    :> QueryParam "offset" AMusicOffset
    :> Header "User-Agent" UserAgent
    :> Header "Authorization" AMusicDeveloperToken
    :> Header "Music-User-Token" AMusicUserToken
    :> Get '[JSON] WAMusic
    -- GET most recently added items, in reverse order; max 150 total, max 25 at a time
    -- https://api.music.apple.com/v1/me/library/recently-added?include=catalog&limit=limit&offset=offset
    :<|> "me"
      :> "library"
      :> "recently-added"
      :> QueryParam "include" AMusicInclude
      :> QueryParam "limit" AMusicLimit
      :> QueryParam "offset" AMusicOffset
      :> Header "User-Agent" UserAgent
      :> Header "Authorization" AMusicDeveloperToken
      :> Header "Music-User-Token" AMusicUserToken
      :> Get '[JSON] WAMusic
getAMusic ::
  Maybe AMusicInclude ->
  Maybe AMusicLimit ->
  Maybe AMusicOffset ->
  Maybe UserAgent ->
  Maybe AMusicDeveloperToken ->
  Maybe AMusicUserToken ->
  ClientM WAMusic
getRecentAMusic ::
  Maybe AMusicInclude ->
  Maybe AMusicLimit ->
  Maybe AMusicOffset ->
  Maybe UserAgent ->
  Maybe AMusicDeveloperToken ->
  Maybe AMusicUserToken ->
  ClientM WAMusic
aMusicAPI :: Proxy AMusicAPI
aMusicAPI = Proxy
getAMusic :<|> getRecentAMusic = client aMusicAPI

-- data AEnv = AEnv { adevt :: AMusicDeveloperToken
--                  , amut :: AMusicUserToken
--                  , alimit :: AMusicLimit
--                  , aoffset :: AMusicOffset
--                  , aclient :: ClientEnv
--                  }

readAMusicReleases :: AMusicInfo -> IO [Release]
readAMusicReleases _ainf = concat <$> unfoldrM (ramr fget) 0
 where
  fget =
    if False
      then getAMusicReleases _ainf -- we just read from cache for now
      else getAMusicReleasesCache

ramr :: (Int -> IO (Maybe WAMusic)) -> Int -> IO (Maybe ([Release], Int))
ramr fget off
  | off >= 9000 = pure Nothing -- max albums to fetch
  | off < 0 = pure Nothing -- emergency stop, needed?
  | otherwise = do
      res <- fget off
      let (rs, next, tot) = maybe ([], -1, 0) getReleases res
      putTextLn $ "-----------------Got " <> show (if next == 0 then off + length rs else next) <> " of " <> show tot <> " Library Albums from Apple Music -----"
      pure $
        if next > 0
          then Just (rs, next)
          else
            if next == 0
              then Just (rs, -1) -- last page
              -- else if next == -1 then Nothing -- error, retry?
              else Nothing

getAMusicReleasesCache :: Int -> IO (Maybe WAMusic)
getAMusicReleasesCache off = do
  let i = off `div` 100
  let fn = "cache/am" <> show i <> ".json"

  let handler :: SomeException -> IO (Either String WAMusic)
      handler ex = pure $ Left ("Caught exception reading file! " <> show ex)
  res <- catch ((eitherDecode <$> readFileLBS fn) :: IO (Either String WAMusic)) handler
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right _ -> pure ()
  pure $ case res of
    Left _ -> Nothing
    Right a -> Just a

getAMusicReleases :: AMusicInfo -> Int -> IO (Maybe WAMusic)
getAMusicReleases ainf off = do
  m <- newManager tlsManagerSettings -- defaultManagerSettings
  let AMusicSession devt mut = ainf
      aquery :: ClientM WAMusic
      aquery =
        if True
          then
            getRecentAMusic -- only recent additions, in increments of 25
              (Just "catalog")
              (Just 25) -- 100
              (Just off)
              (Just "ClutterApp/0.1")
              (Just ("Bearer " <> devt))
              (Just mut)
          else
            getAMusic -- all albums, in increments of 100
              (Just "catalog")
              (Just 100)
              (Just off)
              (Just "ClutterApp/0.1")
              (Just ("Bearer " <> devt))
              (Just mut)
  putTextLn "-----------------Getting Library Albums from Apple Music -----"
  res <- runClientM aquery (mkClientEnv m (BaseUrl Https "api.music.apple.com" 443 "v1"))
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right _ -> pure ()
  pure $ case res of
    Left _ -> Nothing
    Right a -> Just a

getReleases :: WAMusic -> ([Release], Int, Int)
getReleases t = (mapMaybe getRelease ams, nxt, tot)
 where
  WAMusic{data_ = ams, meta_ = meta, next_ = nt} = t
  nxt = if nt == "" then 0 else readInt . toString . snd . T.breakOnEnd "=" $ nt
  WAMMeta{total = tot} = meta
  getRelease :: WAMData -> Maybe Release
  getRelease ti = r
   where
    WAMData
      { id = lid -- library id
      , type__ = ttyp
      , attributes = tattr
      , relationships = trel
      } = ti
    WAMAttr
      { trackCount = tcnt
      , genreNames = gns
      , releaseDate = treleased
      , name = ttitle
      , artistName = tartist
      , artwork = tartwork
      , dateAdded = tcreated
      } = tattr
    WAMArt{url = tcoverurl} = tartwork
    WAMRel{catalog = tcat} = trel
    WAMCat{catdata = cds} = tcat
    (cid, ctcnt) = if null cds then ("", 0) else (cdid, cdtcnt) -- catalog id, trackCount
     where
      WAMCatData{id = cdid, attributes = cdattr} = Unsafe.fromJust $ viaNonEmpty head cds
      WAMCatDataAttr{trackCount = cdtcnt} = cdattr

    r
      | ttyp /= "library-albums" = Nothing
      | tcnt /= ctcnt = Nothing -- only complete albums
      | otherwise =
          Just
            Release
              { daid = if cid == "" then hash . toString $ lid else readInt . toString $ cid
              , dtitle = ttitle
              , dartists = [tartist]
              , dreleased = treleased
              , dadded = tcreated
              , dcover = tcoverurl
              , dfolder = fromEnum TAMusic
              , dformat = "Streaming"
              , dtidalid = Nothing
              , damid = Just cid
              , dqobuzid = Nothing
              , dlocation = Just lid
              , dtags = ["provider.applemusic"] <> map (("genre." <>) . T.toCaseFold) gns
              , drating = 0
              , dplays = 0
              }

