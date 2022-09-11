
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DuplicateRecordFields #-}

module FromAMusic ( readAMusicReleases
                  ,
                  ) where
import Relude
-- import Relude.File

import Network.HTTP.Client ( newManager )
import Network.HTTP.Client.TLS ( tlsManagerSettings )

import Data.Aeson ( (.:), (.:?), (.!=), FromJSON (..), withObject, eitherDecode )
-- import GHC.Generics

-- import Data.Proxy
import Servant
-- import Servant.API
import Servant.Client

import Types ( Release (..), TagFolder (..), AMusicInfo (..) )

data WAMusic = WAMusic
                { meta ::WAMMeta
                , adata :: [WAMItem]
                , next :: !Text
                } deriving (Show, Generic)
instance FromJSON WAMusic
data WAMItem = WAMItem
                { id :: !Text
                } deriving (Show, Generic)
instance FromJSON WAMItem
data WAMMeta = WAMMeta
                { total :: Int
                } deriving (Show, Generic)
instance FromJSON WAMMeta

type UserAgent = Text
type AMusicDeveloperToken = Text
type AMusicUserToken = Text
type AMusicLimit = Int
type AMusicOffset = Int
type AMusicAPI =
-- GET AMusic library albums
-- https://api.music.apple.com/v1/me/library/albums?limit=limit&offset=offset
       "me" :> "library" :> "albums"
       :> QueryParam "limit" AMusicLimit
       :> QueryParam "offset" AMusicOffset
       :> Header "User-Agent" UserAgent
       :> Header "Authorization" AMusicDeveloperToken
       :> Header "Music-User-Token" AMusicUserToken
       :> Get '[JSON] WAMusic
aMusicAPI :: Proxy AMusicAPI
aMusicAPI = Proxy
getAMusic :: Maybe AMusicLimit
          -> Maybe AMusicOffset
          -> Maybe UserAgent
          -> Maybe AMusicDeveloperToken
          -> Maybe AMusicUserToken
          -> ClientM WAMusic
getAMusic = client aMusicAPI

data AEnv = AEnv { adevt :: AMusicDeveloperToken
                 , amut :: AMusicUserToken
                 , alimit :: AMusicLimit
                 , aoffset :: AMusicOffset
                 , aclient :: ClientEnv
                 }
readAMusicReleases :: AMusicInfo -> IO [Release]
readAMusicReleases ainf = do
  m <- newManager tlsManagerSettings  -- defaultManagerSettings
  let AMusicSession devt mut = ainf
      aenv :: AEnv
      aenv = AEnv { adevt = "Bearer " <> devt
                  , amut = mut
                  , alimit = 5
                  , aoffset = 1 -- 1563 -- error at 1565
                  , aclient = mkClientEnv m ( BaseUrl Https "api.music.apple.com" 443 "v1" )
                  }
      aquery :: ClientM WAMusic
      aquery  = getAMusic ( Just (alimit aenv) )
                          ( Just (aoffset aenv) )
                          ( Just "ClutterApp/0.1" )
                          ( Just (adevt aenv) )
                          ( Just (amut aenv) )
  putTextLn "-----------------Getting Library Albums from Apple Music -----"
    {-
  res <- runClientM tquery ( tclient tenv )
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right _ -> pure ()

  let rs = case res of
        Left _ -> []
        Right t -> getReleases t
  -- F.for_ (take 5 rs) (\r -> print $ show (FJ.dtitle r) <> show (FJ.dartists r))

  pure rs
      -}
  pure []


