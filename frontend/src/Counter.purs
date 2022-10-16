module Counter where

import Prelude
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Action = Increment | Decrement

counter :: forall query input output m. H.Component query input output m
counter = H.mkComponent
  { initialState: const 0
  , render
  , eval :  H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
    render state =
      HH.div_
        [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
        , HH.div_ [ HH.text $ show state ]
        , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
        ]
    handleAction = case _ of
      Increment -> H.modify_ \state -> state + 1
      Decrement -> H.modify_ \state -> state - 1


