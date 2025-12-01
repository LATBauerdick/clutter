-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module FromDiscogs (
  -- AppM
  readDiscogsListAids,
  -- IO
  -- readLists,
  readDiscogsReleases,
  readDiscogsReleasesCache,
  readDiscogsRelease,
  readDiscogsLists,
  readDiscogsFolders,
  readDiscogsFoldersCache,
)
where

import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.!=), (.:), (.:?))
import Data.Char as Ch (isDigit)
import qualified Data.Map as M
import qualified Data.Text as T (
  breakOnEnd,
  filter,
  intercalate,
  lines,
  null,
  strip,
  stripPrefix,
  take,
  toCaseFold,
  unpack,
 )
import qualified Data.Text.Read as TR (decimal)
import Data.Vector (Vector)
import qualified Data.Vector as V (empty, fromList)
import GHC.Generics ()
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Relude

-- import Data.Proxy
import Servant

-- import Servant.API
import Servant.Client
import Types (
  Discogs (..),
  Release (..),
  -- AppM,
  -- Env (..),
 )

-- data WTest = WTest
--   { uri :: !Text
--   }
--   deriving (Show, Generic)
newtype WTest = WTest {uri :: Text}
  deriving (Show, Generic)

data WLists = WLists
  { pagination :: WPagination
  , lists :: [WList]
  }
  deriving (Show, Generic)

data WPagination = WPagination
  { pages :: Int
  , items :: Int
  }
  deriving (Show, Generic)

data WList = WList
  { id :: Int
  , name :: !Text
  }
  deriving (Show, Generic)

newtype WFolders = WFolders
  { folders :: [WList]
  }
  deriving (Show, Generic)

data WReleases = WReleases
  { pagination :: WPagination
  , releases :: [WRelease]
  }
  deriving (Show, Generic)

data WRelease = WRelease
  { id :: Int
  , instance_id :: Int
  , date_added :: !Text
  , folder_id :: Int
  , rating :: Int
  , basic_information :: WBasicInfo
  , notes :: [WNote]
  }
  deriving (Show, Generic)

instance FromJSON WRelease where
  parseJSON = withObject "release" $ \o -> do
    daid_ <- o .: "id"
    dinst_ <- o .: "instance_id"
    dadded_ <- o .: "date_added"
    fid_ <- o .: "folder_id"
    rating_ <- o .: "rating"
    bi_ <- o .: "basic_information"
    notes_ <- o .:? "notes" .!= []
    pure $ WRelease daid_ dinst_ dadded_ fid_ rating_ bi_ notes_

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
  { field_id :: Int
  , value :: !Text
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
  , descriptions :: Maybe [Text]
  , text :: Maybe Text
  }
  deriving (Show, Generic)

data WReleases' = WReleases'
  { pagination :: WPagination
  , releases :: [WRelease']
  }
  deriving (Show, Generic)

newtype WRelease' = WRelease'
  { id :: Int
  }
  deriving (Show, Generic)

-- List Items
newtype WLItems = WLItems {wlitems :: [WLAid]} deriving (Show, Generic)

instance FromJSON WLItems where
  parseJSON = withObject "wlitems" $ \o -> do
    d_ <- o .: "items"
    pure $ WLItems d_

data WLAid = WLAid
  { wlaid :: Int
  , wlcomment :: Maybe Text
  }
  deriving (Show)

instance FromJSON WLAid where
  parseJSON = withObject "wlaid" $ \o -> do
    d_ <- o .: "id"
    c_ <- o .:? "comment"
    pure $ WLAid d_ c_

-- WantList Items
data WWList = WWList
  { pagination :: WPagination
  , wants :: [WWItem]
  }
  deriving (Show, Generic)
instance FromJSON WWList

data WWItem = WWItem
  { wwaid :: Int
  , wwdate_added :: !Text
  , wwnotes :: Maybe Text
  }
  deriving (Show)

instance FromJSON WWItem where
  parseJSON = withObject "wants" $ \o -> do
    d1 <- o .: "id"
    d2 <- o .: "date_added"
    d3 <- o .: "notes"
    pure $ WWItem d1 d2 d3

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
    -- Get want list items
    -- "https://api.discogs.com/users/$USR/wants?page=1&per_page=500" -H "Authorization: Discogs token=$TOK"
    :<|> "users"
      :> Capture "name" UserName
      :> "wants"
      :> QueryParam "page" Int
      :> QueryParam "per_page" Int
      :> QueryParam "token" Token
      -- :> Header "Authorization: Discogs token" Token
      :> Header "User-Agent" UserAgent
      :> Get '[JSON] WWList

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
  Int -> -- folder ID
  Maybe Text -> -- sort (label artist title catno format rating added year)
  Maybe Text -> -- sort_order (asc desc)
  Maybe Int -> -- page# 1..
  Maybe Int -> -- per page, max 500?
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
discogsGetWantList ::
  UserName ->
  Maybe Int -> -- page# 1..
  Maybe Int -> -- per page, max 500?
  Maybe Token ->
  Maybe UserAgent ->
  ClientM WWList
discogsAPI :: Proxy DiscogsAPI
discogsAPI = Proxy
discogsGetReleases :<|> discogsGetFolders :<|> discogsGetLists :<|> discogsGetList :<|> discogsGetRelease :<|> discogsGetWantList = client discogsAPI

getWr :: WReleases -> [WRelease]
getWr wr = rs
 where
  WReleases
    { pagination =
      WPagination
        { pages = _
        , items = _
        }
    , releases = rs
    } = wr

lookupName :: Map Text Int -> Int -> Maybe Text
lookupName lns i = fmap fst . find (\(_, li) -> li == i) $ M.toList lns
getR :: Map Text Int -> WRelease -> Release
getR lns dr = r
 where
  WRelease
    { id = did
    , instance_id
    , date_added
    , folder_id
    , rating
    , basic_information =
      WBasicInfo{title, year, cover_image, artists, formats, genres, styles}
    , notes
    } = dr
  as = (\WArtist{name = n} -> n) <$> artists
  nts :: Maybe Text -- Notes field
  nts = case listToMaybe . mapMaybe (\WNote{field_id = i, value = v} -> if i /= 3 then Nothing else Just v) $ notes of
    Just a -> if a /= "" then Just a else Nothing
    _ -> Nothing
  tags :: [Text]
  tags =
    mapMaybe (T.stripPrefix "#")
      . words
      $ fromMaybe "" nts
  -- parse location text (field 4), look for T<tidalID> and A<AppleMusicID> and
  -- Q<QobuzID> or C<Box position>
  loct :: Maybe Text
  loct = case listToMaybe . mapMaybe (\WNote{field_id = i, value = v} -> if i /= 4 then Nothing else Just v) $ notes of
    Just a -> if a /= "" then Just a else Nothing
    _ -> Nothing
  tidalid :: Maybe Text -- T<number>
  -- or https://tidal.com/album/<id>
  tidalid =
    viaNonEmpty
      head
      ( ( mapMaybe (\t -> if T.null (T.filter (not . Ch.isDigit) t) then Just t else Nothing)
            . mapMaybe (fmap (snd . T.breakOnEnd "/") . T.stripPrefix "https://tidal.com/")
            . words
            $ fromMaybe "" loct
        )
          <> ( mapMaybe (\t -> if T.null (T.filter (not . Ch.isDigit) t) then Just t else Nothing)
                . mapMaybe (T.stripPrefix "T")
                . words
                $ fromMaybe "" loct
             )
      )
  amid :: Maybe Text
  -- A<number> -> https://music.apple.com/us/album/<number>
  -- or Al.<string> -> https://music.apple.com/library/albums/l.<string>
  -- or https://music.apple.com/us/album/<...>/<id>
  amid =
    viaNonEmpty
      head
      ( ( mapMaybe (fmap (snd . T.breakOnEnd "/") . T.stripPrefix "https://music.apple.com/us/album/")
            . words
            $ fromMaybe "" loct
        )
          <> ( mapMaybe
                ( \t ->
                    if T.null (T.filter (not . Ch.isDigit) t) || (T.take 2 t == "l.")
                      then Just t
                      else Nothing
                )
                . mapMaybe (T.stripPrefix "A")
                . words
                $ fromMaybe "" loct
             )
      )
  qobuzid :: Maybe Text -- Q<string>
  -- or https://play.qobuz.com/album/<string>
  qobuzid =
    viaNonEmpty
      head
      ( ( mapMaybe (fmap (snd . T.breakOnEnd "/") . T.stripPrefix "https://play.qobuz.com/album/")
            . words
            $ fromMaybe "" loct
        )
          <> (mapMaybe (T.stripPrefix "Q") . words $ fromMaybe "" loct)
      )
  jellyfinid =
    viaNonEmpty
      head
      ( mapMaybe (T.stripPrefix "J") . words $ fromMaybe "" loct
      )
  cdboxid :: Maybe Int
  cdboxid =
    viaNonEmpty head $
      mapMaybe (T.stripPrefix "C" >=> (readMaybe . T.unpack)) $
        words (fromMaybe "" loct)
  -- remove A/T/Q/J<id> and C<id>etc tokens -- probably should use attoparsec instead
  loc :: Maybe Text
  loc = case listToMaybe . mapMaybe (\WNote{field_id = i, value = v} -> if i /= 4 then Nothing else Just v) $ notes of
    Just a ->
      if a /= ""
        then
          Just
            ( unwords
                . mapMaybe (\t -> maybe (Just t) (const Nothing) . T.stripPrefix "https://music.apple.com/us/album/" $ t)
                . mapMaybe (\t -> maybe (Just t) (const Nothing) . T.stripPrefix "https://tidal.com/" $ t)
                . mapMaybe (\t -> maybe (Just t) (const Nothing) . T.stripPrefix "https://play.qobuz.com/album/" $ t)
                . mapMaybe
                  ( \t -> case T.stripPrefix "T" t of
                      Nothing -> Just t
                      Just ta ->
                        if T.null (T.filter (not . Ch.isDigit) ta)
                          then Nothing
                          else Just t
                  )
                . mapMaybe
                  ( \t -> case T.stripPrefix "A" t of
                      Nothing -> Just t
                      Just ta ->
                        if T.null (T.filter (not . Ch.isDigit) ta) || (T.take 2 ta == "l.")
                          then Nothing
                          else Just t
                  )
                . mapMaybe
                  ( \t -> case T.stripPrefix "Q" t of
                      Nothing -> Just t
                      Just _ -> Nothing
                  )
                . mapMaybe
                  ( \t -> case T.stripPrefix "C" t of
                      Nothing -> Just t
                      Just t' -> Just ("Box CDs" <> t')
                  )
                . mapMaybe
                  ( \t -> case T.stripPrefix "J" t of
                      Nothing -> Just t
                      Just _ -> Nothing
                  )
                . words
                $ a
            )
        else Nothing
    _ -> Nothing

  -- parse Order# (field 5)
  _ordn :: Maybe Text
  _ordn = case listToMaybe . mapMaybe (\WNote{field_id = i, value = v} -> if i /= 5 then Nothing else Just v) $ notes of
    Just a -> if a /= "" then Just a else Nothing
    _ -> Nothing

  plays :: Int
  plays = case listToMaybe . mapMaybe (\WNote{field_id = i, value = v} -> if i /= 7 then Nothing else Just v) $ notes of
    Just a -> fromMaybe 0 (readMaybe . toString $ a)
    _ -> 0
  -- format is special for certain folders
  -- should maybe rather go through the "Streaming" and "File" lists and change the format?
  -- xx = \WFormat{name = n, qty = q, descriptions = md, text = mt} -> [n] <> fromMaybe [] md <> maybeToList mt
  _fs :: WFormat -> [Text]
  _fs WFormat{name = n, qty = q, descriptions = md, text = _mt} =
    (if q /= "1" then [n, n <> " x " <> q] else [n]) <> fromMaybe [] md -- <> maybeToList _mt
  fs :: [Text]
  fs = case lookupName lns folder_id of
    -- in case of Straeming, Files, or CDR, ignore Discogs formats
    Just "Streaming" -> one "Streaming"
    Just "Files" -> one "Files"
    Just "CDR" -> one "CDR"
    _ -> concatMap _fs formats
  gs :: [Text]
  gs =
    genres
      -- add genre Piano or Opera if in the corresponding folder, or if Style
      <> case lookupName lns folder_id of
        Just "Piano" -> one "Piano"
        Just "Opera&Vocal" -> one "Opera"
        _ -> []
      <> (maybe [] one . find (("opera" ==) . T.toCaseFold) $ styles)
      <> (maybe [] one . find (("piano" ==) . T.toCaseFold) $ styles)
  -- tags from notes, genres, styles, formats, order#, if there is a tidal or apple music version, discogs
  tagsFormats :: [Text] -> [Text]
  tagsFormats = map (("format." <>) . T.toCaseFold)
  tagsFolder :: Int -> [Text]
  tagsFolder = one . T.toCaseFold . ("folder." <>) . fromMaybe "???" . lookupName lns
  -- add opera if style is opera, and piano if folder is Piano
  tagsGenres :: [Text] -> [Text]
  tagsGenres = map (("genre." <>) . T.toCaseFold)
  tagsRated :: Int -> [Text]
  tagsRated i = case i of
    0 -> one "rated.not"
    1 -> ["rated.", "rated.*", "rated.dislike"]
    2 -> ["rated.", "rated.**", "rated.dislike"]
    3 -> ["rated.", "rated.***"]
    4 -> ["rated.", "rated.****", "rated.like"]
    _ -> ["rated.", "rated.*****", "rated.like"]
  tagsPlays :: Int -> [Text]
  tagsPlays i = case i of
    0 -> one "played.never"
    1 -> ["played.", "played.once"]
    2 -> ["played.", "played.twice"]
    _ -> ["played.", "played.often"]
  tagsProvider = ["provider.discogs"] <> maybe [] (const ["provider.applemusic"]) amid <> maybe [] (const ["provider.tidal"]) tidalid <> maybe [] (const ["provider.qobuz"]) qobuzid <> maybe [] (const ["provider.jellyfin"]) jellyfinid <> maybe [] (const ["provider.cdbox"]) cdboxid <> maybe [] (const ["provider.local"]) (if loc == Just "" then Nothing else loc)

  tagsList :: [Text]
  tagsList = sortNub $ tagsProvider <> tagsFormats fs <> tags <> tagsGenres gs <> map T.toCaseFold styles <> tagsPlays plays <> tagsRated rating <> tagsFolder folder_id
  r =
    Release
      { daid = did
      , dinst = instance_id
      , dtitle = title
      , dartists = as
      , dreleased = show year
      , dadded = date_added
      , dcover = cover_image
      , dfolder = folder_id
      , dformat = T.intercalate ", " fs
      , dtidalid = tidalid
      , dqobuzid = qobuzid
      , djellyfinid = jellyfinid
      , damid = amid
      , dlocIdx = cdboxid
      , dlocation = if loc == Just "" then Nothing else loc
      , dtags = tagsList
      , drating = rating
      , dplays = plays
      }

getToken :: Discogs -> (Text, Text)
getToken di = case di of
  DiscogsSession tok un -> (tok, un)
  _ -> ("", "")

releasesFromDiscogsApi :: Discogs -> Int -> IO (Either String [WRelease])
releasesFromDiscogsApi di nreleases = do
  m <- newManager tlsManagerSettings -- defaultManagerSettings
  let (tok, un) = getToken di
  let dc = mkClientEnv m discogsBaseUrl
      query :: ClientM [WRelease]
      query =
        if nreleases == 0
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
          else do
            -- only read nreleases, newest first
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
  res <- liftIO $ releasesFromCacheFile fn
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right _ -> pure ()
  let rs = case res of
        Left _ -> []
        Right d -> getR lns <$> d
  pure rs

-- getting n Discogs Releases, all if n == 0
readDiscogsReleases :: Discogs -> Map Text Int -> Int -> IO [Release]
readDiscogsReleases di lns n = do
  putTextLn "-----------------Getting Releases from Discogs-----"
  res <- liftIO $ releasesFromDiscogsApi di n
  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right _ -> pure ()
  let rs = case res of
        Left _ -> []
        Right d -> getR lns <$> d
  pure rs

readDiscogsRelease :: Discogs -> Map Text Int -> Int -> IO (Maybe Release)
readDiscogsRelease di lns rid = do
  res <- liftIO $ releaseFromDiscogsApi di rid
  case res of
    Left err -> putTextLn $ "Error in readDiscogsRelease: " <> show err
    Right _ -> pure ()
  pure $ case res of
    Left _ -> Nothing
    Right d -> Just (getR lns d)

releaseFromDiscogsApi :: Discogs -> Int -> IO (Either String WRelease)
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

listsFromDiscogsApi :: Discogs -> IO (Either String WLists)
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

readDiscogsLists :: Discogs -> IO (Map Text (Int, Vector Int))
readDiscogsLists di = do
  res <- case di of
    DiscogsFile fn -> do
      putTextLn "-----------------Getting Lists from Discogs Cache-----"
      listsFromCacheFile fn
    _ -> do
      putTextLn "-----------------Getting Lists from Discogs-----"
      listsFromDiscogsApi di

  case res of
    Left err -> putTextLn $ "Error: " <> show err
    Right _ -> pure ()
  let ls :: [WList]
      ls = case res of
        Left _ -> []
        Right wls -> lists wls
  let getAids :: Discogs -> WList -> IO (Text, (Int, Vector Int))
      getAids dd WList{id = i, name = n} = do
        -- lists are returend empty and read when used, unless cached...
        is <- case dd of
          DiscogsFile _ -> readDiscogsListAids dd i
          _ -> pure V.empty
        pure (n, (i, is))
  -- let lm :: [ ( Text, (Int, Vector Int) ) ]
  lm <- traverse (getAids di) ls

  -- let lm :: [(Text, (Int, Vector Int))]
  --     lm = case di of
  --       DiscogsFile _ -> do
  --         (\WList{id = i, name = n} -> (n, (i, V.empty))) <$> ls
  --       _ ->
  --         -- lists are returend empty unless cached...
  --         (\WList{id = i, name = n} -> (n, (i, V.empty))) <$> ls

  -- ...except the Wantlist and Box CDs
  -- wl <- readWantListAids di

  wl' <- readBoxListAids di

  pure . M.fromList $ lm <> one ("Want", (7, V.empty)) <> one ("Box CDs", (1627102, wl'))

listsFromCacheFile :: FilePath -> IO (Either String WLists)
listsFromCacheFile fn = eitherDecode <$> readFileLBS (fn <> "lists-raw.json") :: IO (Either String WLists)

foldersFromDiscogsApi :: Discogs -> IO (Either String WFolders)
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

readDiscogsFolders :: Discogs -> IO (Map Text Int)
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
      fm = (\WList{id = i, name = n} -> (n, i)) <$> fs
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
      fm = (\WList{id = i, name = n} -> (n, i)) <$> fs
  pure $ M.fromList fm

-- for each Discog list, read the lists of album ids from JSON
-- we're treating Discog folders like lists,
-- also assuming that their IDs are unique
-- NB: the JSON required to extract album id info is different between them
readWLItemsCache :: FilePath -> IO (Either String WLItems)
readWLItemsCache fn = (eitherDecode <$> readFileLBS fn) :: IO (Either String WLItems)

readWLItems :: Text -> Int -> IO (Either String WLItems)
readWLItems tok i = do
  m <- newManager tlsManagerSettings
  let dc = mkClientEnv m discogsBaseUrl
  res <- runClientM (discogsGetList i (Just tok) userAgent) dc
  pure $ case res of
    Left err -> Left ("Error: " <> show err)
    Right x -> Right x

-- create list of [ <albumID>, (locList name, locList position) ]
-- dicsogsLocList :: (Text, (Int, Vector Int)) -> [(Int, (Text, Int))]
-- dicsogsLocList (ln, (1627102, aids)) = []
-- discogsLocList (ln, (_, aids)) = zipWith (\idx aid -> (aid, (ln, idx))) [1 ..] (V.toList aids)

readDiscogsListAids :: Discogs -> Int -> IO (Vector Int)
readDiscogsListAids di i = do
  case i of
    1627102 -> readBoxListAids di
    7 -> readWantListAids di
    _ -> do
      res <- case di of
        DiscogsFile fn -> do
          let fn' = fn <> "l" <> show i <> "-raw.json"
          readWLItemsCache fn'
        DiscogsSession tok _ -> readWLItems tok i
      case res of
        Left err -> putTextLn $ "Error: " <> show err
        Right _ -> pure ()

      -- F.traverse_ print $ take 5 . wlitems $ ls
      let aids = wlaid <$> V.fromList (wlitems (fromRight (WLItems []) res))
      pure aids

sortByNums :: [] Int -> [Maybe Text] -> [] Int
sortByNums aids nums = fst <$> sortOn snd pairs
 where
  pairs :: [(Int, Int)]
  pairs =
    concatMap
      ( \(aid, maybeNums) ->
          case maybeNums of
            Just numsText
              | not (T.null numsText) ->
                  -- let individualNums = mapMaybe (either (const Nothing) (Just . fst) . TR.decimal . T.strip) . T.splitOn "," $ numsText
                  let individualNums = mapMaybe (either (const Nothing) (Just . fst) . TR.decimal . T.strip) [numsText]
                   in map (\d -> (aid, d)) individualNums
            -- _ -> [] -- Nothing or empty text or not a number -> exclude this ID
            _ -> [(aid, 0)]
      )
      (zip aids nums)

readBoxListAids :: Discogs -> IO (Vector Int)
readBoxListAids di = do
  let i = 1627102
  res <- case di of
    DiscogsFile fn -> do
      putTextLn "-----------------Getting Box List from Discogs Cache-----"
      let fn' = fn <> "l" <> show i <> "-raw.json"
      readWLItemsCache fn'
    DiscogsSession tok _ -> do
      putTextLn "-----------------Getting Box List from Discogs-----"
      readWLItems tok i

  case res of
    Left err -> putTextLn $ "Reading BoxList: " <> show err
    Right _ -> pure ()
  let wls :: [WLAid]
      wls = case res of
        Left _ -> []
        Right wl -> wlitems wl
  let aids = wlaid <$> wls
  let cs = wlcomment <$> wls
  -- putTextLn "-----------------readBoxListAids"
  -- print $ zip aids cs
  -- print $ sortByNums aids cs
  pure . V.fromList $ sortByNums aids cs

sortByMultipleDates :: [] Int -> [Maybe Text] -> [] Int
sortByMultipleDates aids dates = fst <$> sortBy (comparing (Down . snd)) pairs
 where
  pairs =
    concatMap
      ( \(aid, maybeDatesText) ->
          case maybeDatesText of
            Just datesText
              | not (T.null datesText) ->
                  let individualDates = T.lines datesText
                   in map (\d -> (aid, d)) individualDates
            _ -> [] -- Nothing or empty text -> exclude this ID
      )
      (zip aids dates)

readWantListAids :: Discogs -> IO (Vector Int)
readWantListAids di = do
  res <- case di of
    DiscogsFile fn -> do
      putTextLn "-----------------Getting Want List from Discogs Cache-----"
      let fn' = fn <> "wants1.json"
      readWWItemsCache fn'
    DiscogsSession tok un -> do
      putTextLn "-----------------Getting Want List from Discogs-----"
      readWWItems tok un
  case res of
    Left err -> putTextLn $ "Reading Wantlist: " <> show err
    Right _ -> pure ()
  let wls :: [WWItem]
      wls = case res of
        Left _ -> []
        Right wl -> wants wl
  let aids = wwaid <$> wls
  let notess = wwnotes <$> wls
  let dates = wwdate_added <$> wls
  -- print $ zip3 aids dates notess
  -- print $ sortByMultipleDates aids notess
  pure . V.fromList $ sortByMultipleDates aids notess

readWWItemsCache :: FilePath -> IO (Either String WWList)
readWWItemsCache fn = (eitherDecode <$> readFileLBS fn) :: IO (Either String WWList)

readWWItems :: Text -> Text -> IO (Either String WWList)
readWWItems tok un = do
  m <- newManager tlsManagerSettings
  let dc = mkClientEnv m discogsBaseUrl
  let query :: ClientM WWList
      query = discogsGetWantList un (Just 1) (Just 500) (Just tok) userAgent
  res <- liftIO $ runClientM query dc
  pure $ case res of
    Left err -> Left ("Error: " <> show err)
    Right x -> Right x
