{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module FromTidal (
  readTidalReleases,
  readTidalReleasesCache,
) where

import Relude

-- import Relude.File

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.!=), (.:), (.:?))

-- import GHC.Generics

-- import Data.Proxy
import Servant

-- import Servant.API
import Servant.Client

import Types (Release (..), TagFolder (..), TidalInfo (..))

{-
https://github.com/yaronzz/Tidal-Media-Downloader/blob/master/TIDALDL-PY/tidal_dl/tidal.py

__URL_PRE__ = 'https://api.tidalhifi.com/v1/'
__AUTH_URL__ = 'https://auth.tidal.com/v1/oauth2'
# known API key for Fire Stick HD(MQA, Dolby Vision enabled)
__API_KEY__ = {'clientId': 'aR7gUaTK1ihpXOEP', 'clientSecret': 'eVWBEkuL2FCjxgjOkR3yK0RYZEbcrMXRc2l8fU3ZCdE='}

    def getDeviceCode(self):
        data = {
            'client_id': __API_KEY__['clientId'],
            'scope': 'r_usr+w_usr+w_sub'
        }
        e, result = self.__post__(__AUTH_URL__ + '/device_authorization', data)
        if e is not None:
            return str(e), False

        if 'status' in result and result['status'] != 200:
            return "Device authorization failed. Please try again.", False

        self.key.deviceCode = result['deviceCode']
        self.key.userCode = result['userCode']
        self.key.verificationUrl = result['verificationUri']
        self.key.authCheckTimeout = result['expiresIn']
        self.key.authCheckInterval = result['interval']
        return None, True
 - -}

data WTidal = WTidal
  { limit :: Int
  , totalNumberOfItems :: Int
  , items :: [WTItem]
  }
  deriving (Show, Generic)
instance FromJSON WTidal
data WTItem = WTItem
  { created :: !Text
  , item :: WTIItem
  }
  deriving (Show, Generic)
instance FromJSON WTItem
data WTIItem = WTIItem
  { id :: Int
  , title :: !Text
  , releaseDate :: !Text
  , url :: !Text
  , cover :: !Text
  , artists :: [WTArtist]
  }
  deriving (Show, Generic)
instance FromJSON WTIItem where
  parseJSON = withObject "wtiitem" $ \o -> do
    id_ <- o .: "id"
    title_ <- o .: "title"
    releaseDate_ <- o .: "releaseDate"
    url_ <- o .: "url"
    cover_ <- o .:? "cover" .!= ""
    artists_ <- o .: "artists"
    pure $ WTIItem id_ title_ releaseDate_ url_ cover_ artists_

data WTArtist = WTArtist
  { id :: Int
  , name :: !Text
  }
  deriving (Show, Generic)
instance FromJSON WTArtist

type UserAgent = Text
type TidalSessionId = Text
type TidalAccessToken = Text
type TidalUserId = Int
type TidalCountryCode = Text
type TidalLimit = Int
type TidalOffset = Int
type TidalAPI =
  -- GET Tidal favorites
  -- https://api.tidalhifi.com/v1/users/<userId>/favorites/albums?sessionId=<session-id>&countryCode=US&limit=2999&offset=0
  "users"
    :> Capture "uid" TidalUserId
    :> "favorites"
    :> "albums"
    :> QueryParam "sessionId" TidalSessionId
    :> QueryParam "countryCode" TidalCountryCode
    :> QueryParam "limit" TidalLimit
    :> QueryParam "offset" TidalOffset
    :> Header "User-Agent" UserAgent
    :> Header "Authorization" TidalAccessToken
    :> Get '[JSON] WTidal
tidalAPI :: Proxy TidalAPI
tidalAPI = Proxy
getTidal ::
  TidalUserId ->
  Maybe TidalSessionId ->
  Maybe TidalCountryCode ->
  Maybe TidalLimit ->
  Maybe TidalOffset ->
  Maybe UserAgent ->
  Maybe TidalAccessToken ->
  ClientM WTidal
getTidal = client tidalAPI

data TEnv = TEnv
  { userId :: TidalUserId
  , sessionId :: TidalSessionId
  , countryCode :: TidalCountryCode
  , accessToken :: TidalAccessToken
  , tlimit :: TidalLimit
  , toffset :: TidalOffset
  , tclient :: ClientEnv
  }
readTidalReleases :: TidalInfo -> IO [Release]
readTidalReleases tinf = do
  m <- newManager tlsManagerSettings -- defaultManagerSettings
  let (uid, sid, cc, at) = case tinf of
        TidalSession uid_ sid_ cc_ at_ -> (uid_, sid_, cc_, at_)
        _ -> (0, "", "", "") -- this cannot happen, I think...
      tenv :: TEnv
      tenv =
        TEnv
          { userId = uid
          , sessionId = sid
          , countryCode = cc
          , accessToken = "Bearer " <> at
          , tlimit = 3999 -- 5
          , toffset = 0 -- 1563 -- error at 1565
          , tclient = mkClientEnv m (BaseUrl Https "api.tidalhifi.com" 443 "v1")
          }
      tquery :: ClientM WTidal
      tquery =
        getTidal
          (userId tenv)
          (Just (sessionId tenv))
          (Just (countryCode tenv))
          (Just (tlimit tenv))
          (Just (toffset tenv))
          (Just "ClutterApp/0.1")
          (Just (accessToken tenv))
  putTextLn "-----------------Getting Favorite Albums from Tidal-----"
  res <- runClientM tquery (tclient tenv)
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right _ -> pure ()

  let rs = case res of
        Left _ -> []
        Right t -> getReleases t
  -- F.for_ (take 5 rs) (\r -> print $ show (FJ.dtitle r) <> show (FJ.dartists r))

  pure rs

readTidalReleasesCache :: FilePath -> IO [Release]
readTidalReleasesCache fn = do
  res <- releasesFromCacheFile fn
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right _ -> pure ()

  let rs = case res of
        Left _ -> []
        Right t -> getReleases t

  pure rs

releasesFromCacheFile :: FilePath -> IO (Either String WTidal)
releasesFromCacheFile fn = do
  putTextLn "-----------------Getting Favorite Albums from Tidal Cache-----"
  (eitherDecode <$> readFileLBS fn) :: IO (Either String WTidal)

getReleases :: WTidal -> [Release]
getReleases t = getRelease <$> tis
 where
  WTidal{items = tis} = t
  getRelease :: WTItem -> Release
  getRelease ti = r
   where
    WTItem{created = tcreated, item = tii} = ti
    WTIItem
      { id = tid
      , title = ttitle
      , releaseDate = treleased
      , url = _turl
      , cover = tcover
      , artists = tartists
      } = tii
    as = (\WTArtist{name = n} -> n) <$> tartists
    r =
      Release
        { daid = tid
        , dinst = 0
        , dtitle = ttitle
        , dartists = as
        , dreleased = treleased
        , dadded = tcreated
        , dcover = tcover
        , dfolder = fromEnum TTidal
        , dformat = "Streaming"
        , dtidalid = Just (show tid)
        , damid = Nothing
        , dqobuzid = Nothing
        , djellyfinid = Nothing
        , dlocIdx = Nothing
        , dlocation = Nothing
        , dtags = ["provider.tidal"]
        , drating = 0
        , dplays = 0
        }
