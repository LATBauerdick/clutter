module GetStuff ( getNow
                , getUrl
                , _encodeURIComponent
                , pushHistory
                , historyBack
                , historyForward
                , initHistory
                , onPopState
                ) where

import Prelude
import Affjax.Node as AN
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..))
import Effect (Effect)
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

-- browser history, so that Back returns to the view before (AlbumComponent keeps the views)
-- every entry carries its index, so that a move is known to be Back or Forward
foreign import initHistory :: Effect Unit
foreign import pushHistory :: Int -> Effect Unit
foreign import historyBack :: Effect Unit
foreign import historyForward :: Effect Unit
foreign import onPopState :: (Int -> Effect Unit) -> Effect Unit
