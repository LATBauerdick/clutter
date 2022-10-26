module RenderTopMenu (
  renderTopMenu
) where

import Prelude

-- import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Data.Foldable (intercalate)

import Types (SortOrder (..), MenuState)

renderTopMenu :: forall w' i'. MenuState -> HH.HTML w' i'
renderTopMenu state =
  HH.div
    [ HP.id "navbar" ]
    [ HH.div [HP.class_ $ HH.ClassName "dropdown"] renderShow
    , HH.div [HP.class_ $ HH.ClassName "dropdown"] renderFocus
    , HH.div [HP.class_ $ HH.ClassName "dropdown"] renderButtonSort
    , HH.div [HP.class_ $ HH.ClassName "dropdown"] [renderButtonOrder]
    , HH.a   [ HP.class_ $ HH.ClassName "active"
             , HP.href (uhq <> "2022 Listened?&sortBy=Default&sortOrder=Desc")] [ HH.text "Listened" ]
    , HH.a   [ HP.class_ $ HH.ClassName "active"
             , HP.href (uhq <> "Discogs")] [ HH.text "Discogs" ]
    , HH.div [HP.class_ $ HH.ClassName "dropdown"] renderButtonList
    , HH.div [HP.class_ $ HH.ClassName "dropdown"] renderButtonLocation
    , HH.div [HP.class_ $ HH.ClassName "dropdown"] renderButtonTags
    ]
  where

  uhq = state.uhq -- "localhost:8080/albums/"
  ln = state.ln
  ffs = state.ffs
  sorts = state.sorts
  sortName = state.sortName
  sso = state.sso
  sts = state.sts
  listNames = state.listNames
  locNames = state.locNames


  renderShow :: forall w i. Array (HH.HTML w i)
  renderShow = [
    HH.button [HP.class_ $ HH.ClassName "dropbtn"] [
      HH.a [ HP.class_ $ HH.ClassName "dropbtn"
           , HP.href ("http:8080/albums/" <> ln)
           ]
           [ HH.text $ "Showing " <> ln ]
    ]
  ]

  renderFocus :: forall w i. Array (HH.HTML w i)
  renderFocus =
    let
      lnk :: forall ww ii. String -> HH.HTML ww ii
      lnk t = HH.a [ HP.class_ $ HH.ClassName tc
                   ,HP.href (uhq <> ln)
                   ]
                   [ HH.text t ]
      p = false -- this tag was selected
      pp = true -- tag was True
      tc :: String
      tc
                      | p && pp   = "focus-on"
                      | p         = "focus-not"
                      | otherwise = "focus"
    in [
      HH.button [HP.class_ $ HH.ClassName "dropbtn"]
      [ HH.a [ HP.class_ $ HH.ClassName "dropbtn"
             , HP.href (uhq <> ln <> "?focus=%23folder.pop" )
             ]
             [ HH.text $ if  ffs /= []
                          then "Focus (#" <> intercalate " #" ffs <> ")"
                          else "Focus "
             , HH.i [ HP.class_ $ HH.ClassName "fa fa-caret-down" ] []
             ]
      ]
      , HH.div [HP.class_ $ HH.ClassName "focus-content"]
          (map lnk sts)
    ]


  renderButtonSort :: forall w i. Array (HH.HTML w i)
  renderButtonSort = [
    HH.button [HP.class_ $ HH.ClassName "dropbtn"]
    [ HH.text $ "Sort " <> if sortName /= "Default"
                            then "by " <> sortName <> " "
                            else ""
    ]
    , HH.div [HP.class_ $ HH.ClassName "dropdown-content"]
        ( map (\x -> HH.a [ HP.href (uhq <> ln <> "&sortBy=" <> x) ] [ HH.text x ]) sorts )
  ]

  renderButtonOrder :: forall w i. HH.HTML w i
  renderButtonOrder =
    HH.button [HP.class_ $ HH.ClassName "dropbtn-order"]
    [ HH.a [ HP.href (uhq <> ln) ]
           [ case sso of
               Asc  -> HH.i [ HP.class_ $ HH.ClassName "fa fa-chevron-circle-down" ] []
               Desc -> HH.i [ HP.class_ $ HH.ClassName "fa fa-chevron-circle-up" ] []
           ]
    ]

  renderButtonList :: forall w i. Array (HH.HTML w i)
  renderButtonList = [
    HH.button [HP.class_ $ HH.ClassName "dropbtn"]
    [ HH.text "List "
    , HH.i [ HP.class_ $ HH.ClassName "fa fa-caret-down" ] []
    ]
  , HH.div [HP.class_ $ HH.ClassName "dropdown-content"]
      ( map (\x -> HH.a [ HP.href (uhq <> "#xxx" <> x) ] [ HH.text x ]) listNames )
  ]

  renderButtonLocation :: forall w i. Array (HH.HTML w i)
  renderButtonLocation = [
    HH.button [HP.class_ $ HH.ClassName "dropbtn"]
    [ HH.text "Location "
    , HH.i [ HP.class_ $ HH.ClassName "fa fa-caret-down" ] []
    ]
  , HH.div [HP.class_ $ HH.ClassName "dropdown-content"]
      ( map (\x -> HH.a [ HP.href (uhq <> "" <> x) ] [ HH.text x ]) locNames )
  ]

  renderButtonTags :: forall w i. Array (HH.HTML w i)
  renderButtonTags = [
    HH.button [HP.class_ $ HH.ClassName "dropbtn"]
    [ HH.text "Tags "
    , HH.i [ HP.class_ $ HH.ClassName "fa fa-caret-down" ] []
    ]
  , HH.div [HP.class_ $ HH.ClassName "dropdown-content"]
      ( map (\x -> HH.a [ HP.href (uhq <> "#xxx" <> x) ] [ HH.text x ]) sts )
  ]
