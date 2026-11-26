module GetStuff ( getNow
                , getUrl
                , postStrings
                , _encodeURIComponent
                , pushHistory
                , historyBack
                , historyForward
                , initHistory
                , onPopState
                , isClamped
                ) where

import Prelude
import Affjax.Node as AN
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Data.Maybe (Maybe(..), fromMaybe)
import Affjax.RequestBody as RequestBody
import Data.Argonaut.Encode (encodeJson)
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

-- POST a list of strings as a JSON array (many album keys do not fit in a URL)
postStrings :: String -> Array String -> Aff String
postStrings url xs = do
  result <- AN.post ResponseFormat.string url (Just (RequestBody.json (encodeJson xs)))
  pure case result of
           Left err -> "POST / api response failed to decode: " <> AN.printError err
           Right response -> response.body

foreign import _encodeURIComponent :: String -> String

-- browser history, so that Back returns to the view before (AlbumComponent keeps the views)
-- every entry carries its index, so that a move is known to be Back or Forward
foreign import initHistory :: Effect Unit
foreign import pushHistory :: Int -> Effect Unit
foreign import historyBack :: Effect Unit
foreign import historyForward :: Effect Unit
foreign import onPopState :: (Int -> Effect Unit) -> Effect Unit

-- is the element (a CSS selector) cut off by its line clamp?
foreign import isClamped :: String -> Effect Boolean
