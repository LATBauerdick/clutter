{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module FromDiscogs
  (
  -- AppM
    readListAids,
  -- IO
    readLists,
    readDiscogsReleases,
    readDiscogsReleasesCache,
    readDiscogsRelease,
    readDiscogsLists,
    readDiscogsListsCache,
    readDiscogsFolders,
    readDiscogsFoldersCache,
  )
where

import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.!=), (.:), (.:?))
import qualified Data.Map as M
import Data.Vector (Vector)
import qualified Data.Vector as V (empty, fromList)
import GHC.Generics ()
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Relude
import Data.Text as T (stripPrefix, filter, null, toCaseFold
                      , take, intercalate, breakOnEnd )
import Data.Char as Ch (isDigit)

-- import Data.Proxy
import Servant
-- import Servant.API
import Servant.Client
import Types
  ( DiscogsInfo (..),
    -- Discogs (..),
    Release (..),
    -- AppM,
    -- Env (..),
  )

-- data WTest = WTest
--   { uri :: !Text
--   }
--   deriving (Show, Generic)
newtype WTest
     = WTest {uri :: Text}
     deriving (Show, Generic)

data WLists = WLists
  { pagination :: WPagination,
    lists :: [WList]
  }
  deriving (Show, Generic)

data WPagination = WPagination
  { pages :: Int,
    items :: Int
  }
  deriving (Show, Generic)

data WList = WList
  { id :: Int,
    name :: !Text
  }
  deriving (Show, Generic)

newtype WFolders = WFolders
  { folders :: [WList]
  }
  deriving (Show, Generic)

data WReleases = WReleases
  { pagination :: WPagination,
    releases :: [WRelease]
  }
  deriving (Show, Generic)

data WRelease = WRelease
  { id :: Int,
    date_added :: !Text,
    folder_id :: Int,
    rating :: Int,
    basic_information :: WBasicInfo,
    notes :: [WNote]
  }
  deriving (Show, Generic)

instance FromJSON WRelease where
  parseJSON = withObject "release" $ \o -> do
    daid_ <- o .: "id"
    dadded_ <- o .: "date_added"
    fid_ <- o .: "folder_id"
    rating_ <- o .: "rating"
    bi_ <- o .: "basic_information"
    notes_ <- o .:? "notes" .!= []
    pure $ WRelease daid_ dadded_ fid_ rating_ bi_ notes_

data WBasicInfo = WBasicInfo
  { title :: !Text
  , year :: Int
  , cover_image :: !Text
  , artists :: [WArtist]
  , formats :: [WFormat]
  , genres :: [Text]
  , styles :: [Text]
  }
  deriving (Show, Generic)

data WNote = WNote
  { field_id :: Int,
    value :: !Text
  }
  deriving (Show, Generic)

data WArtist = WArtist
  { name :: !Text
  , id :: Int
  }
  deriving (Show, Generic)

data WFormat = WFormat
  { name :: !Text
  , qty :: !Text
  }
  deriving (Show, Generic)

data WReleases' = WReleases'
  { pagination :: WPagination,
    releases :: [WRelease']
  }
  deriving (Show, Generic)

newtype WRelease' = WRelease'
  { id :: Int
  }
  deriving (Show, Generic)

newtype WLItems = WLItems {wlitems :: [WAid]} deriving (Show, Generic)

instance FromJSON WLItems where
  parseJSON = withObject "wlitems" $ \o -> do
    d_ <- o .: "items"
    pure $ WLItems d_

newtype WAid = WAid {wlaid :: Int} deriving (Show)

instance FromJSON WAid where
  parseJSON = withObject "waid" $ \o -> do
    d_ <- o .: "id"
    pure $ WAid d_

instance FromJSON WTest

instance FromJSON WLists

instance FromJSON WFolders

instance FromJSON WList

instance FromJSON WPagination

instance FromJSON WReleases

-- instance FromJSON WRelease
instance FromJSON WBasicInfo

instance FromJSON WNote

instance FromJSON WArtist

instance FromJSON WFormat

instance FromJSON WReleases'

instance FromJSON WRelease'

type Token = Text

type UserName = Text

type UserAgent = Text

userAgent :: Maybe Text
userAgent = Just "ClutterApp/0.1 +http://bauerdick.org/clutter"

discogsBaseUrl :: BaseUrl
discogsBaseUrl = BaseUrl Https "api.discogs.com" 443 []

type DiscogsAPI =
  -- GET release
  -- "releases/249504"
  -- :> Header "Authorization: Discogs token" Token
  -- :> Header "User-Agent" UserAgent
  -- :> Get '[JSON] WTest
  -- GET releases
  "users"
    :> Capture "name" UserName
    :> "collection"
    :> "folders"
    :> Capture "folder_id" Int
    :> "releases"
    :> QueryParam "sort" Text
    :> QueryParam "sort_order" Text
    :> QueryParam "page" Int
    :> QueryParam "per_page" Int
    :> QueryParam "token" Token
    -- :> Header "Authorization: Discogs token" Token
    :> Header "User-Agent" UserAgent
    :> Get '[JSON] WReleases
    -- GET folders
    :<|> "users"
      :> Capture "name" UserName
      :> "collection"
      :> "folders"
      :> QueryParam "token" Token
      -- :> Header "Authorization: Discogs token" Token
      :> Header "User-Agent" UserAgent
      :> Get '[JSON] WFolders
    -- GET lists
    :<|> "users"
      :> Capture "name" UserName
      :> "lists"
      :> QueryParam "token" Token
      -- :> Header "Authorization: Discogs token" Token
      :> Header "User-Agent" UserAgent
      :> Get '[JSON] WLists
    -- Get list items
    :<|> "lists"
      :> Capture "listid" Int
      :> QueryParam "token" Token
      -- :> Header "Authorization: Discogs token" Token
      :> Header "User-Agent" UserAgent
      :> Get '[JSON] WLItems
    -- Get release item
    :<|> "users"
      :> Capture "name" UserName
      :> "collection"
      :> "releases"
      :> Capture "releaseid" Int
      :> QueryParam "token" Token
      -- :> Header "Authorization: Discogs token" Token
      :> Header "User-Agent" UserAgent
      :> Get '[JSON] WReleases

--
-- Get Folder items
-- :<|> "users"
--      :> Capture "name" UserName
--      :> "collection" :> "folders"
--      :> Capture "folder_id" Int
--      :> "releases"
--      :> QueryParam "token" Token
--      -- :> Header "Authorization: Discogs token" Token
--      :> Header "User-Agent" UserAgent
--      :> Get '[JSON] WReleases'

discogsGetReleases ::
  UserName ->
  Int ->        -- folder ID
  Maybe Text -> -- sort (label artist title catno format rating added year)
  Maybe Text -> -- sort_order (asc desc)
  Maybe Int ->  -- page# 1..
  Maybe Int ->  -- per page, max 500?
  Maybe Token ->
  Maybe UserAgent ->
  ClientM WReleases
discogsGetFolders ::
  UserName ->
  Maybe Token ->
  Maybe UserAgent ->
  ClientM WFolders
discogsGetLists ::
  UserName ->
  Maybe Token ->
  Maybe UserAgent ->
  ClientM WLists
discogsGetList ::
  Int ->
  Maybe Token ->
  Maybe UserAgent ->
  ClientM WLItems
discogsGetRelease ::
  UserName ->
  Int ->
  Maybe Token ->
  Maybe UserAgent ->
  ClientM WReleases
discogsAPI :: Proxy DiscogsAPI
discogsAPI = Proxy
discogsGetReleases :<|> discogsGetFolders :<|> discogsGetLists :<|> discogsGetList :<|> discogsGetRelease = client discogsAPI

getWr :: WReleases -> [WRelease]
getWr wr = rs
  where
    WReleases
      { pagination =
          WPagination
            { pages = _,
              items = _
            },
        releases = rs
      } = wr

getR :: (Int -> Maybe Text) -> WRelease -> Release
getR folderName dr = r
  where
    WRelease
      { id = did,
        date_added = da,
        folder_id = dfolder_id,
        rating = drat,
        basic_information =
          WBasicInfo
            { title = dt,
              year = dyear,
              cover_image = dcov,
              artists = das,
              formats = dfs,
              genres = dgens,
              styles = dstls
            },
        notes = ns
      } = dr
    as = (\WArtist {name = n} -> n) <$> das
    nts :: Maybe Text -- Notes field
    nts = case listToMaybe . mapMaybe (\WNote {field_id = i, value = v} -> if i /= 3 then Nothing else Just v) $ ns of
      Just a -> if a /= "" then Just a else Nothing
      _ -> Nothing
    tags :: [Text]
    tags = mapMaybe (stripPrefix "#")
         . words
         $ fromMaybe "" nts
    -- parse location text (field 4), look for T<tidalID> and A<AppleMusicID>
    loct :: Maybe Text
    loct = case listToMaybe . mapMaybe (\WNote {field_id = i, value = v} -> if i /= 4 then Nothing else Just v) $ ns of
      Just a -> if a /= "" then Just a else Nothing
      _ -> Nothing
    tidalid :: Maybe Text  -- T<number>
    -- or https://tidal.com/album/<id>
    tidalid =
      viaNonEmpty head (
        ( mapMaybe (\t -> if T.null (T.filter (not . Ch.isDigit) t) then Just t else Nothing)
        . mapMaybe ( fmap ( snd . T.breakOnEnd "/" ) . T.stripPrefix  "https://tidal.com/" )
        . words
        $ fromMaybe "" loct
        ) <> (
        mapMaybe (\t -> if T.null (T.filter (not . Ch.isDigit) t) then Just t else Nothing)
        . mapMaybe (T.stripPrefix "T")
        . words
        $ fromMaybe "" loct
        ))
    amid :: Maybe Text
    -- A<number> -> https://music.apple.com/us/album/<number>
    -- or Al.<string> -> https://music.apple.com/library/albums/l.<string>
    -- or https://music.apple.com/us/album/<...>/<id>
    amid =
      viaNonEmpty head ((
        mapMaybe ( fmap ( snd . T.breakOnEnd "/" ) . T.stripPrefix  "https://music.apple.com/us/album/" )
        . words
        $ fromMaybe "" loct
        ) <> (
        mapMaybe  (\t ->  if T.null (T.filter (not . Ch.isDigit) t) || (T.take 2 t == "l.")
                                then Just t
                                else Nothing
                    )
          . mapMaybe (T.stripPrefix "A")
          . words
          $ fromMaybe "" loct
        )
      )
    -- remove A<id> and T<id> tokens -- probably should use attoparsec instead
    _xxx :: Maybe Text
    _xxx = case listToMaybe . mapMaybe (\WNote {field_id = i, value = v} -> if i /= 4 then Nothing else Just v) $ ns of
      Just a -> if a /= "" then Just a else Nothing
      _ -> Nothing
    loc :: Maybe Text
    loc = case listToMaybe . mapMaybe (\WNote {field_id = i, value = v} -> if i /= 4 then Nothing else Just v) $ ns of
      Just a -> if a /= "" then Just (unwords
                                    . mapMaybe (\t -> maybe (Just t) (const Nothing) . T.stripPrefix "https://music.apple.com/us/album/" $ t)
                                    . mapMaybe (\t -> maybe (Just t) (const Nothing) . T.stripPrefix "https://tidal.com/" $ t)
                                    . mapMaybe (\t -> case T.stripPrefix "T" t of
                                        Nothing -> Just t
                                        Just ta -> if T.null (T.filter (not . Ch.isDigit) ta)
                                                    then Nothing
                                                    else Just t)
                                    . mapMaybe  (\t -> case stripPrefix "A" t of
                                        Nothing -> Just t
                                        Just ta ->  if T.null (T.filter (not . Ch.isDigit) ta) || (T.take 2 ta == "l.")
                                                      then Nothing
                                                      else Just t
                                                )
                                    . words
                                    $ a)
                          else Nothing
      _ -> Nothing

    -- parse Order# (field 5)
    _ordn :: Maybe Text
    _ordn = case listToMaybe . mapMaybe (\WNote {field_id = i, value = v} -> if i /= 5 then Nothing else Just v) $ ns of
      Just a -> if a /= "" then Just a else Nothing
      _ -> Nothing

    plays :: Int
    plays = case listToMaybe . mapMaybe (\WNote {field_id = i, value = v} -> if i /= 7 then Nothing else Just v) $ ns of
      Just a -> fromMaybe 0 (readMaybe . toString $ a)
      _ -> 0
-- format is special for certain folders
-- should maybe rather go through the "Streaming" and "File" lists and change the format?
    fs :: [Text]
    fs = case folderName dfolder_id of
           Just "Streaming" -> one "Streaming"
           Just "Files"     -> one "Files"
           _                -> (\WFormat {name = n} -> n) <$> dfs
    -- tags from notes, genres, styles, formats, order#, if there is a tidal or apple music version, discogs
    tagsFormats :: [Text] -> [Text]
    tagsFormats = map (("format." <>) . T.toCaseFold)
    tagsFolder :: Int -> [Text]
    tagsFolder = one . T.toCaseFold . ("folder." <>) . fromMaybe "???" . folderName
    -- add opera if style is opera
    tagsGenres :: [Text] -> [Text]
    tagsGenres ts = map (("genre." <>) . T.toCaseFold)
                  $ ts <> (maybe [] one
                          . find (( "opera" == ) . T.toCaseFold)
                          $ dstls)
    tagsRated :: Int -> [Text]
    tagsRated i = case i of
      0 -> one "rated.not"
      1 -> ["rated.", "rated.*"]
      2 -> ["rated.", "rated.**"]
      3 -> ["rated.", "rated.***"]
      4 -> ["rated.", "rated.****"]
      _ -> ["rated.", "rated.*****"]
    tagsPlays :: Int -> [Text]
    tagsPlays i = case i of
      0 -> one "played.never"
      1 -> ["played.", "played.once"]
      2 -> ["played.", "played.twice"]
      _ -> ["played.", "played.often"]
    tagsProvider = ["provider.discogs"] <> maybe [] (const ["provider.applemusic"]) amid <> maybe [] (const ["provider.tidal"]) tidalid <> maybe [] (const ["provider.local"]) (if loc == Just "" then Nothing else loc)

    tagsList :: [Text]
    tagsList = sortNub $ tagsProvider <> tagsFormats fs <> tags <> tagsGenres dgens <> map T.toCaseFold dstls <> tagsPlays plays <> tagsRated drat <> tagsFolder dfolder_id
    r =
      Release
        { daid      = did,
          dtitle    = dt,
          dartists  = as,
          dreleased = show dyear,
          dadded    = da,
          dcover    = dcov,
          dfolder   = dfolder_id,
          dformat   = T.intercalate ", " fs,
          dtidalid  = tidalid,
          damid     = amid,
          dlocation = if loc == Just "" then Nothing else loc,
          dtags     = tagsList,
          drating   = drat,
          dplays    = plays
        }

getToken :: DiscogsInfo -> (Text, Text)
getToken di = case di of
  DiscogsSession tok un -> (tok, un)
  _ -> ("", "")

releasesFromDiscogsApi :: DiscogsInfo -> Int -> IO (Either String [WRelease])
releasesFromDiscogsApi di nreleases = do
  m <- newManager tlsManagerSettings -- defaultManagerSettings
  let (tok, un) = getToken di
  let dc = mkClientEnv m discogsBaseUrl
      query :: ClientM [WRelease]
      query = if nreleases == 0
        then do
          r0 <- discogsGetReleases un 0 Nothing Nothing (Just 1) (Just 500) (Just tok) userAgent
          let rs0 = getWr r0
          r1 <- discogsGetReleases un 0 Nothing Nothing (Just 2) (Just 500) (Just tok) userAgent
          let rs1 = getWr r1
          r2 <- discogsGetReleases un 0 Nothing Nothing (Just 3) (Just 500) (Just tok) userAgent
          let rs2 = getWr r2
          r3 <- discogsGetReleases un 0 Nothing Nothing (Just 4) (Just 500) (Just tok) userAgent
          let rs3 = getWr r3
          pure $ rs0 <> rs1 <> rs2 <> rs3
        else do -- only read nreleases, newest first
          r0 <- discogsGetReleases un 0 (Just "added") (Just "desc") (Just 1) (Just nreleases) (Just tok) userAgent
          let rs0 = getWr r0
          pure rs0
  putTextLn $ "-----------------Getting Collection from Discogs (asked for " <> show nreleases <> " releases)-----"
  res <- runClientM query dc
  case res of
    Left err -> pure $ Left (show err)
    Right r -> pure $ Right r

releasesFromCacheFile :: FilePath -> IO (Either String [WRelease])
releasesFromCacheFile fn = do
  putTextLn "-----------------Getting Collection from Discogs Cache-----"
  res0 <- (eitherDecode <$> readFileLBS (fn <> "draw1.json")) :: IO (Either String WReleases)
  res1 <- (eitherDecode <$> readFileLBS (fn <> "draw2.json")) :: IO (Either String WReleases)
  res2 <- (eitherDecode <$> readFileLBS (fn <> "draw3.json")) :: IO (Either String WReleases)
  res3 <- (eitherDecode <$> readFileLBS (fn <> "draw4.json")) :: IO (Either String WReleases)
  pure . Right . concatMap getWr . rights $ [res0, res1, res2, res3]

readDiscogsReleasesCache :: FilePath -> Map Text Int -> IO [Release]
readDiscogsReleasesCache fn lns = do
  let ln :: Int -> Maybe Text; ln i = fmap fst . find (\(_, li) -> li == i) $ M.toList lns
  res <- liftIO $ releasesFromCacheFile fn
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right _ -> pure ()
  let rs = case res of
        Left _ -> []
        Right d -> getR ln <$> d
  pure rs

-- getting n Discogs Releases, all if n == 0
readDiscogsReleases :: DiscogsInfo -> Map Text Int -> Int -> IO [Release]
readDiscogsReleases di lns n = do
  putTextLn "-----------------Getting Releases from Discogs-----"
  let ln :: Int -> Maybe Text; ln i = fmap fst . find (\(_, li) -> li == i) $ M.toList lns
  res <- liftIO $ releasesFromDiscogsApi di n
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right _ -> pure ()
  let rs = case res of
        Left _ -> []
        Right d -> getR ln <$> d
  pure rs

readDiscogsRelease :: DiscogsInfo -> Map Text Int -> Int -> IO (Maybe Release)
readDiscogsRelease di lns rid = do
  let ln :: Int -> Maybe Text; ln i = fmap fst . find (\(_, li) -> li == i) $ M.toList lns
  res <- liftIO $ releaseFromDiscogsApi di rid
  case res of
    Left err -> putTextLn $ "Error in readDiscogsRelease: " <> show err
    Right _ -> pure ()
  pure $ case res of
    Left _ -> Nothing
    Right d -> Just (getR ln d)

releaseFromDiscogsApi :: DiscogsInfo -> Int -> IO (Either String WRelease)
releaseFromDiscogsApi di aid = do
  m <- newManager tlsManagerSettings -- defaultManagerSettings
  let (tok, un) = getToken di
  let dc = mkClientEnv m discogsBaseUrl
      query :: ClientM WReleases
      query = do
        discogsGetRelease un aid (Just tok) userAgent
  putTextLn $ "-----------------Getting Release " <> show aid <> " from Discogs-----"
  res <- runClientM query dc
  case res of
    Left err -> pure $ Left (show err)
    Right rs -> pure $ case viaNonEmpty head (getWr rs) of
      Nothing -> Left $ "No Release Found for " <> show aid
      Just r -> Right r

listsFromDiscogsApi :: DiscogsInfo -> IO (Either String WLists)
listsFromDiscogsApi di = do
  m <- newManager tlsManagerSettings -- defaultManagerSettings
  let (tok, un) = getToken di
  let dc = mkClientEnv m discogsBaseUrl
  -- get list and folder names and ids
  let query :: ClientM WLists
      query = discogsGetLists un (Just tok) userAgent
  res <- runClientM query dc
  pure $ case res of
    Left err -> Left (show err)
    Right r -> Right r

readDiscogsLists :: DiscogsInfo -> IO (Map Text (Int, Vector Int))
readDiscogsLists di = do
  putTextLn "-----------------Getting Lists from Discogs-----"
  res <- listsFromDiscogsApi di
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right _ -> pure ()
  let ls = case res of
        Left _ -> []
        Right wls -> lists wls

  let lm :: [(Text, (Int, Vector Int))]
      lm = (\WList {id = i, name = n} -> (n, (i, V.empty))) <$> ls
  pure $ M.fromList lm

listsFromCacheFile :: FilePath -> IO (Either String WLists)
listsFromCacheFile fn = eitherDecode <$> readFileLBS (fn <> "lists-raw.json") :: IO (Either String WLists)

readDiscogsListsCache :: FilePath -> IO (Map Text (Int, Vector Int))
readDiscogsListsCache fn = do
  putTextLn "-----------------Getting Lists from Discogs Cache-----"
  res <- listsFromCacheFile fn
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right _ -> pure ()
  let ls = case res of
        Left _ -> []
        Right wls -> lists wls -- [WList]
  let getAids :: FilePath -> WList -> IO (Text, (Int, Vector Int))
      getAids f WList {id = i, name = n} = do
        is <- readListAidsCache f i
        pure (n, (i, is))

  -- let lm :: [ ( Text, (Int, Vector Int) ) ]
  lm <- traverse (getAids fn) ls

  pure $ M.fromList lm

foldersFromDiscogsApi :: DiscogsInfo -> IO (Either String WFolders)
foldersFromDiscogsApi di = do
  m <- newManager tlsManagerSettings
  let (tok, un) = getToken di
      dc = mkClientEnv m discogsBaseUrl
  -- get list and folder names and ids
  res <- runClientM (discogsGetFolders un (Just tok) userAgent) dc
  pure $ case res of
    Left err -> Left (show err)
    Right r -> Right r

foldersFromCacheFile :: FilePath -> IO (Either String WFolders)
foldersFromCacheFile fn =
  (eitherDecode <$> readFileLBS (fn <> "folders-raw.json")) :: IO (Either String WFolders)

readDiscogsFolders :: DiscogsInfo -> IO (Map Text Int)
readDiscogsFolders di = do
  -- get list and folder names and ids
  putTextLn "-----------------Getting Folders from Discogs-----"
  res <- foldersFromDiscogsApi di
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right _ -> do pure ()
  let fs :: [WList]
      fs = case res of
        Left _ -> []
        Right wfs -> folders wfs
  let fm :: [(Text, Int)]
      fm = (\WList {id = i, name = n} -> (n, i)) <$> fs
  pure $ M.fromList fm

readDiscogsFoldersCache :: FilePath -> IO (Map Text Int)
readDiscogsFoldersCache fn = do
  -- get list and folder names and ids
  putTextLn "-----------------Getting Folders from Discogs Cache-----"
  res <- foldersFromCacheFile fn
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right _ -> do pure ()
  let fs :: [WList]
      fs = case res of
        Left _ -> []
        Right wfs -> folders wfs
  let fm :: [(Text, Int)]
      fm = (\WList {id = i, name = n} -> (n, i)) <$> fs
  pure $ M.fromList fm

-- for each Discog list, read the lists of album ids from JSON
-- we're treating Discog folders like lists,
-- also assuming that their IDs are unique
-- NB: the JSON required to extract album id info is different between them
readListAids :: DiscogsInfo -> Int -> IO (Vector Int)
readListAids di i = do
  let (tok, _) = getToken di
  m <- liftIO $ newManager tlsManagerSettings
  let dc = mkClientEnv m discogsBaseUrl
  let query :: ClientM WLItems
      query = discogsGetList i (Just tok) userAgent
  res <- liftIO $ runClientM query dc
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right _ -> pure ()
  -- F.traverse_ print $ take 5 . wlitems $ ls
  let aids = wlaid <$> V.fromList (wlitems (fromRight (WLItems []) res))
  pure aids

readListAidsCache :: FilePath -> Int -> IO (Vector Int)
readListAidsCache fn i = do
  putTextLn $ "-----------------Getting List " <> show i <> " from Discogs Cache-----"
  -- res <- runClientM ( discogsGetList i ( Just tok ) userAgent ) dc
  let fn' = fn <> "l" <> show i <> "-raw.json"
  res <- readWLItemsCache fn'
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right _ -> pure ()
  -- F.traverse_ print $ take 5 . wlitems $ ls
  let aids = wlaid <$> V.fromList (wlitems (fromRight (WLItems []) res))
  pure aids

readWLItemsCache :: FilePath -> IO (Either String WLItems)
readWLItemsCache fn = (eitherDecode <$> readFileLBS fn) :: IO (Either String WLItems)

readLists :: DiscogsInfo -> IO (Map Text (Int, Vector Int))
readLists di = do
  let (tok, un) = getToken di
  m <- newManager tlsManagerSettings -- defaultManagerSettings
  let dc = mkClientEnv m discogsBaseUrl
  let query :: ClientM WLists
      query = discogsGetLists un (Just tok) userAgent
  putTextLn "-----------------reading Lists from Discogs-----"
  res <- runClientM query dc
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right _ -> pure ()
  let ls = case res of
        Left _ -> []
        Right wls -> lists wls
  -- map with all lists
  let lm :: Map Text (Int, Vector Int)
      lm = M.fromList . map (\WList {id = i, name = n} -> (n, (i, V.empty))) $ ls
  pure lm
