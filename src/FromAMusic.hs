
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
import qualified Relude.Unsafe as Unsafe

import Data.Hashable (hash)

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
                {
                  data_ :: [WAMData]
                , meta_ :: WAMMeta
                , next :: !Text
                } deriving (Show, Generic)
instance FromJSON WAMusic where
  parseJSON = withObject "wamusic" $ \ o -> do
    d_      <- o .: "data"
    m_      <- o .: "meta"
    n_      <- o .: "next"
    pure $ WAMusic  d_ m_ n_
data WAMData = WAMData
                { id :: !Text
                , attributes :: WAMAttr
                , relationships :: WAMRel
                } deriving (Show, Generic)
instance FromJSON WAMData
data WAMMeta = WAMMeta
                { total :: Int
                } deriving (Show, Generic)
instance FromJSON WAMMeta
data WAMAttr = WAMAttr
                { name :: !Text
                , artistName :: !Text
                , artwork :: WAMArt
                , dateAdded :: !Text
                , releaseDate :: !Text
                } deriving (Show, Generic)
instance FromJSON WAMAttr
data WAMArt = WAMArt
                { width :: Int
                , height :: Int
                , url :: !Text
                } deriving (Show, Generic)
instance FromJSON WAMArt
data WAMRel = WAMRel
                { catalog :: WAMCat
                } deriving (Show, Generic)
instance FromJSON WAMRel
data WAMCat = WAMCat
                { catdata :: [WAMCatData]
                } deriving (Show, Generic)
instance FromJSON WAMCat where
  parseJSON = withObject "wamcat" $ \ o -> do
    d_      <- o .: "data"
    pure $ WAMCat  d_
data WAMCatData = WAMCatData
                { id :: !Text
                } deriving (Show, Generic)
instance FromJSON WAMCatData

type UserAgent = Text
type AMusicInclude = Text
type AMusicDeveloperToken = Text
type AMusicUserToken = Text
type AMusicLimit = Int
type AMusicOffset = Int
type AMusicAPI =
-- GET AMusic library albums
-- https://api.music.apple.com/v1/me/library/albums?include=catalog&limit=limit&offset=offset
       "me" :> "library" :> "albums"
       :> QueryParam "include" AMusicInclude
       :> QueryParam "limit" AMusicLimit
       :> QueryParam "offset" AMusicOffset
       :> Header "User-Agent" UserAgent
       :> Header "Authorization" AMusicDeveloperToken
       :> Header "Music-User-Token" AMusicUserToken
       :> Get '[JSON] WAMusic
aMusicAPI :: Proxy AMusicAPI
aMusicAPI = Proxy
getAMusic :: Maybe AMusicInclude
          -> Maybe AMusicLimit
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
                  , alimit = 50
                  , aoffset = 90 -- 1563 -- error at 1565
                  , aclient = mkClientEnv m ( BaseUrl Https "api.music.apple.com" 443 "v1" )
                  }
      aquery :: ClientM WAMusic
      aquery  = getAMusic
                          ( Just "catalog" )
                          ( Just (alimit aenv) )
                          ( Just (aoffset aenv) )
                          ( Just "ClutterApp/0.1" )
                          ( Just (adevt aenv) )
                          ( Just (amut aenv) )
  putTextLn "-----------------Getting Library Albums from Apple Music -----"
  res <- runClientM aquery ( aclient aenv )
  -- print res
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right _ -> pure ()
  let (rs, tot) = case res of
        Left _ -> ([],0)
        Right a -> getReleases a
  -- print rs
  putTextLn $ "-----------------Got " <> show (length rs) <> " of " <> show tot <> " Library Albums from Apple Music -----"
  pure rs


getReleases :: WAMusic -> ([Release], Int)
getReleases t = (getRelease <$> tis, tot) where
        WAMusic {data_=tis, meta_=meta } = t
        WAMMeta {total=tot} = meta
        getRelease :: WAMData -> Release
        getRelease ti = r where
          WAMData { id = tid
                  , attributes = tattr
                  , relationships = trel
                  } = ti
          WAMAttr { name = ttitle
                  , artistName = tartist
                  , artwork = tartwork
                  , releaseDate = treleased
                  , dateAdded = tcreated
                  } = tattr
          WAMArt { url = tcoverurl } = tartwork
          WAMRel { catalog = tcat } = trel
          WAMCat { catdata = cds } = tcat
          cid = if length cds == 0 then "" else ccc where
            WAMCatData { id = ccc  } = Unsafe.fromJust $ viaNonEmpty head cds

          tfolder = fromEnum TAMusic

          r = Release  { daid      = hash $ toString tid --Q!!!!!
                       , dtitle    = ttitle
                       , dartists  = [tartist]
                       , dreleased = treleased
                       , dadded    = tcreated
                       , dcover    = tcoverurl
                       , dfolder   = tfolder
                       , dformat   = "Streaming"
                       , dtidalid  = Nothing
                       , damid     = Just cid
                       , dlocation = Nothing
                       , dtags     = ["provider.appleMusic"]
                       , drating   = 0
                       , dplays    = 0
                       }

