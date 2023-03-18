module Main (main) where

import App (app)
import qualified Data.ByteString.Char8 as S8
import Data.Text (Text)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Env (envInit)
import Provider (readAlbums)
import Relude hiding (get)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Types (Discogs (..), Env (..), Tidal (..), TidalInfo (..))

main :: IO ()
main = do
  t <- readFileBS "data/tok.dat"
  let ts :: [Text]; ts = words . decodeUtf8 $ t
      appleMusicDevToken = fromJust $ ts !!? 0 -- t6
      appleMusicUserToken = fromJust $ ts !!? 1 -- t7
      discogsDevToken = fromJust $ ts !!? 2
      discogsUserName = fromJust $ ts !!? 3
      tidalUserId = fromMaybe 0 $ readMaybe (toString . fromJust $ ts !!? 4) :: Int
      tidalSessionId = fromJust $ ts !!? 5 -- t3
      tidalCountryCode = fromJust $ ts !!? 6 -- t4
      tidalAccessToken = fromJust $ ts !!? 7 -- t5
  -- vta <- readAlbums $ Tidal $ TidalSession tidalUserId tidalSessionId tidalCountryCode tidalAccessToken
  testEnv <- envInit True

  let spec :: Spec
      spec = with (return (app testEnv)) $ do
        describe "GET /albums/Listened" $ do
          it "responds with 200" $ do
            get "/albums/Listened" `shouldRespondWith` 200

  -- describe "GET /provider/discogs/<discogs-token>/<discogs-user>" $ do
  --   it "responds with 200" $ do
  --     get (S8.pack ("/provider/discogs/" ++ toString discogsToken ++ "/" ++ toString discogsUser)) `shouldRespondWith` 200

  hspec spec
