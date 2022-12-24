
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

import qualified Data.Text as T
import Data.Hashable (hash)
import Control.Monad.Loops (unfoldrM)

import Network.HTTP.Client ( newManager )
import Network.HTTP.Client.TLS ( tlsManagerSettings )

import Data.Aeson ( (.:), (.:?), (.!=), FromJSON (..), withObject )
-- import GHC.Generics

-- import Data.Proxy
import Servant
-- import Servant.API
import Servant.Client

import Types ( Release (..), TagFolder (..), AMusicInfo (..) )

readInt :: String -> Int -- crash if not an integer
readInt = Unsafe.fromJust . readMaybe

data WAMusic = WAMusic
                {
                  data_ :: [WAMData]
                , meta_ :: WAMMeta
                , next_ :: !Text
                } deriving (Show, Generic)
instance FromJSON WAMusic where
  parseJSON = withObject "wamusic" $ \ o -> do
    d_      <- o .:  "data"
    m_      <- o .:  "meta"
    n_      <- o .:? "next" .!= ""
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
                { trackCount :: Int
                , genreNames :: [Text]
                , releaseDate :: !Text
                , name :: !Text
                , artistName :: !Text
                , artwork :: WAMArt
                , dateAdded :: !Text
                } deriving (Show, Generic)
instance FromJSON WAMAttr where
  parseJSON = withObject "attr" $ \o -> do
    trackCount_  <- o .: "trackCount"
    genreNames_  <- o .: "genreNames"
    releaseDate_ <- o .:? "releaseDate" .!= ""
    name_        <- o .: "name"
    artistName_  <- o .: "artistName"
    artwork_     <- o .:? "artwork" .!= WAMArt 0 0 ""
    dateAdded_   <- o .: "dateAdded"
    pure $ WAMAttr trackCount_ genreNames_ releaseDate_ name_ artistName_ artwork_ dateAdded_
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
                , attributes :: WAMCatDataAttr
                } deriving (Show, Generic)
instance FromJSON WAMCatData
data WAMCatDataAttr = WAMCatDataAttr
                { trackCount :: Int
                } deriving (Show, Generic)
instance FromJSON WAMCatDataAttr

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
     --  :<|> me/library/recently-added?include=catalog
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
readAMusicReleases inf = concat <$> unfoldrM (ramr inf) 0

ramr :: AMusicInfo -> Int -> IO (Maybe ( [Release] , Int))
ramr ainf off
 | off >= 9000 = pure Nothing -- max number of albums to fetch
 | off == -1 = pure Nothing
 | otherwise = do
    m <- newManager tlsManagerSettings  -- defaultManagerSettings
    let AMusicSession devt mut = ainf
        aenv :: AEnv
        aenv = AEnv { adevt = "Bearer " <> devt
                    , amut = mut
                    , alimit = 100
                    , aoffset = off
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
    let (rs, next, tot) = case res of
          Left _ -> ([],-1,0)
          Right a -> getReleases a
    -- print rs
    putTextLn $ "-----------------Got " <> show (if next == 0 then off + length rs else next) <> " of " <> show tot <> " Library Albums from Apple Music -----"
    pure $ if next > 0 then
                        Just (rs, next)
                          else if next == 0 then
                            Just (rs, -1) -- last page
                              else if next == -1 then
                                Nothing -- should check if retry makes sense; for the moment, just stop
                                -- Just ([], off) -- error, try again
                                  else
                                    Nothing -- this should never happen


getReleases :: WAMusic -> ([Release], Int, Int)
getReleases t = (mapMaybe getRelease ams, nxt, tot) where
        WAMusic {data_=ams, meta_=meta, next_=nt} = t
        nxt = if nt == "" then 0 else readInt . toString . snd . T.breakOnEnd "=" $ nt
        WAMMeta {total=tot} = meta
        getRelease :: WAMData -> Maybe Release
        getRelease ti = r where
          WAMData { id = lid  -- library id
                  , attributes = tattr
                  , relationships = trel
                  } = ti
          WAMAttr { trackCount = tcnt
                  , genreNames = gns
                  , releaseDate = treleased
                  , name = ttitle
                  , artistName = tartist
                  , artwork = tartwork
                  , dateAdded = tcreated
                  } = tattr
          WAMArt { url = tcoverurl } = tartwork
          WAMRel { catalog = tcat } = trel
          WAMCat { catdata = cds } = tcat
          (cid, ctcnt) = if length cds == 0 then ("", 0) else (cdid, cdtcnt) where -- catalog id, trackCount
            WAMCatData { id = cdid, attributes = cdattr } = Unsafe.fromJust $ viaNonEmpty head cds
            WAMCatDataAttr { trackCount = cdtcnt } = cdattr

          r = if tcnt /= ctcnt then Nothing else -- only complete albums
            Just
              Release  { daid      = if cid == "" then hash . toString $ lid else readInt . toString $ cid
                       , dtitle    = ttitle
                       , dartists  = [tartist]
                       , dreleased = treleased
                       , dadded    = tcreated
                       , dcover    = tcoverurl
                       , dfolder   = fromEnum TAMusic
                       , dformat   = "Streaming"
                       , dtidalid  = Nothing
                       , damid     = Just cid
                       , dlocation = Just lid
                       , dtags     = ["provider.applemusic"] <> map (("genre." <>) . T.toCaseFold) gns
                       , drating   = 0
                       , dplays    = 0
                       }

