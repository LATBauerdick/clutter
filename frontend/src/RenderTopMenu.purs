module RenderTopMenu (
  renderTopMenu
) where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Data.Foldable (intercalate)

renderTopMenu :: forall w i. HH.HTML w i
renderTopMenu =
  HH.div
    [ HP.id "navbar" ]
    [ HH.div [HP.class_ $ HH.ClassName "dropdown"] [renderShow]
    , HH.div [HP.class_ $ HH.ClassName "dropdown"] [renderFocus]
    , HH.div [HP.class_ $ HH.ClassName "dropdown"] [renderButtonSort]
    , HH.div [HP.class_ $ HH.ClassName "dropdown"] [renderButtonOrder]
    , HH.a   [ HP.class_ $ HH.ClassName "active"
             , HP.href (uhq <> "2022 Listened?&sortBy=Default&sortOrder=Desc")] [ HH.text "Listened" ]
    , HH.a   [ HP.class_ $ HH.ClassName "active"
             , HP.href (uhq <> "Discogs")] [ HH.text "Discogs" ]
    , HH.div [HP.class_ $ HH.ClassName "dropdown"] [renderButtonList]
    , HH.div [HP.class_ $ HH.ClassName "dropdown"] [renderButtonLocation]
    , HH.div [HP.class_ $ HH.ClassName "dropdown"] [renderButtonTags]
    ]
  where
  uhq = "localhost:8080/albums/"
  ln = "Discogs" -- list
  fs = [ "pop", "classical" ] -- focus
  ffs = [ "pop" ]
  aids = [ 18520963, 659642 ] -- album ids
  sorts = [ "Added", "Artist", "Default", "Title" ]
  sortName = "Default"

  renderShow :: forall w i. HH.HTML w i
  renderShow =
    HH.button [HP.class_ $ HH.ClassName "dropbtn"] [
      HH.a [ HP.class_ $ HH.ClassName "dropbtn"
           , HP.href ("http:8080/albums/" <> ln)
           ]
           [ HH.text $ "Showing " <> ln ]
    ]

  renderFocus :: forall w i. HH.HTML w i
  renderFocus =
    HH.button [HP.class_ $ HH.ClassName "dropbtn"] [
      HH.a [ HP.class_ $ HH.ClassName "dropbtn"
           , HP.href (uhq <> ln <> "?focus=%23folder.pop" )
           ]
           [ HH.text $ if  ffs /= []
                        then "Focus (#" <> intercalate " #" ffs <> ")"
                        else "Focus "
           , HH.i [ HP.class_ $ HH.ClassName "fa fa-caret-down" ] []
           ]
    ]

  renderButtonSort :: forall w i. HH.HTML w i
  renderButtonSort = 
    HH.button [HP.class_ $ HH.ClassName "dropbtn"]
    [ HH.text $ "Sort " <> if sortName /= "Default"
                            then "by " <> sortName <> " "
                            else ""
    , HH.div [HP.class_ $ HH.ClassName "dropdown-content"]
        (map HH.text sorts)
    ]

  renderButtonOrder :: forall w i. HH.HTML w i
  renderButtonOrder = HH.text "OO"
  renderButtonList :: forall w i. HH.HTML w i
  renderButtonList = HH.text "List"
  renderButtonLocation :: forall w i. HH.HTML w i
  renderButtonLocation = HH.text "Location"
  renderButtonTags :: forall w i. HH.HTML w i
  renderButtonTags = HH.text "Tags"
