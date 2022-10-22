module GetStuff ( getNow
                , getUrl
                , _encodeURIComponent
                ) where

import Prelude
import Affjax.Node as AN
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Data.Maybe (fromMaybe)
import Effect.Now (nowDateTime, getTimezoneOffset)
import Data.DateTime (DateTime, adjust)
import Data.Time.Duration (negateDuration)
import Effect.Class (liftEffect)

getNow :: Aff DateTime
getNow = do
  utc <- liftEffect nowDateTime
  dt <- liftEffect getTimezoneOffset
  pure $ fromMaybe utc $ adjust (negateDuration dt) utc

getUrl :: String -> Aff String
getUrl url = do
  result <- AN.get ResponseFormat.string url
  pure case result of
           Left err -> "GET / api response failed to decode: " <> AN.printError err
           Right response -> response.body

foreign import _encodeURIComponent :: String -> String
