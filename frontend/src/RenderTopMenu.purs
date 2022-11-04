module RenderTopMenu (
  renderTopMenu
) where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))

import Types (SortOrder (..), State, AlbumList(..),  Action(..))

renderTopMenu :: forall m. State -> H.ComponentHTML Action () m
renderTopMenu state =
  HH.div
    [ HP.id "navbar" ]
    [ HH.div [HP.class_ $ HH.ClassName "dropdown"] renderShow
    , HH.div [HP.class_ $ HH.ClassName "dropdown"] renderFocus
    , HH.div [HP.class_ $ HH.ClassName "dropdown"] renderButtonSort
    , HH.div [HP.class_ $ HH.ClassName "dropdown"] [renderButtonOrder]
    -- , HH.a   [ HP.class_ $ HH.ClassName "active"
    --          , HP.href (uhq <> "2022 Listened?&sortBy=Default&sortOrder=Desc")
    --          ]
    --          [ HH.text "Listened" ]
    -- , HH.a   [ HP.class_ $ HH.ClassName "active"
    --          , HP.href (uhq <> "Discogs")
    --          ]
    --          [ HH.text "Discogs" ]
    , HH.div [HP.class_ $ HH.ClassName "dropdown"]
      [ HH.button
        [ HP.class_ $ HH.ClassName "dropbtn1"
        , HE.onClick \ev -> ShowList $ AlbumList (Just "2022 Listened?&sortBy=Default&sortOrder=Desc")
        , HP.type_ HP.ButtonSubmit
        , HP.disabled state.loading
        ]
        [ HH.text "Listened" ]
      ]
    , HH.div [HP.class_ $ HH.ClassName "dropdown"]
      [ HH.button
        [ HP.class_ $ HH.ClassName "dropbtn1"
        , HE.onClick \ev -> ShowList $ AlbumList (Just "Discogs")
        , HP.type_ HP.ButtonSubmit
        , HP.disabled state.loading
        ]
        [ HH.text "Discogs" ]
      ]
    , HH.div [HP.class_ $ HH.ClassName "dropdown"] renderButtonList
    , HH.div [HP.class_ $ HH.ClassName "dropdown"] renderButtonLocation
    , HH.div [HP.class_ $ HH.ClassName "dropdown"] renderButtonTags
    ]
  where

  uhq = state.menu.params.muhq -- "localhost:8080/albums/"
  sorts = state.menu.params.msorts
  sts = state.menu.params.msts
  listNames = state.menu.params.mlistNames
  locNames = state.menu.params.mlocNames

  sn = state.menu.sortName
  ln =  state.menu.ln
  ffs = state.menu.ffs
  sso = state.menu.sso

  renderShow :: forall w i. Array (HH.HTML w i)
  renderShow = [
    HH.button
    [ HP.class_ $ HH.ClassName "dropbtn"
    ]
    [
      HH.a [ HP.class_ $ HH.ClassName "dropbtn"
           , HP.href (uhq <> ln)
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
    [ HH.text $ "Sort " <> if sn /= "Default"
                            then "by " <> sn <> " "
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

  renderButtonList :: forall m. Array ( H.ComponentHTML Action () m )
  renderButtonList =
    [
      HH.button
      [ HP.class_ $ HH.ClassName "dropbtn"]
      [ HH.text "List "
      , HH.i [ HP.class_ $ HH.ClassName "fa fa-caret-down" ] []
      ]
    , HH.div [HP.class_ $ HH.ClassName "dropdown-content"]
        ( map (\x -> HH.button [ HP.class_ $ HH.ClassName "xxx"
                               , HE.onClick (\ev -> ShowList ( AlbumList (Just x) ))
                               ]
                               [ HH.text x ])
                     listNames )
    ]

  renderButtonLocation :: forall m. Array ( H.ComponentHTML Action () m )
  renderButtonLocation = [
    HH.button [HP.class_ $ HH.ClassName "dropbtn"]
    [ HH.text "Location "
    , HH.i [ HP.class_ $ HH.ClassName "fa fa-caret-down" ] []
    ]
  , HH.div [HP.class_ $ HH.ClassName "dropdown-content"]
        ( map (\x -> HH.button [ HP.class_ $ HH.ClassName "dropdown"
                               , HE.onClick (\ev -> ShowList ( AlbumList (Just x) ))
                               ]
                               [ HH.text x ])
                     locNames )
      -- ( map (\x -> HH.a [ HP.href (uhq <> "" <> x) ] [ HH.text x ]) locNames )
      -- ( map (\x -> HH.text x ) locNames )
  ]

  renderButtonTags :: forall w i. Array (HH.HTML w i)
  renderButtonTags = [
    HH.button [HP.class_ $ HH.ClassName "dropbtn"]
    [ HH.text "Tags "
    , HH.i [ HP.class_ $ HH.ClassName "fa fa-caret-down" ] []
    ]
  , HH.div [HP.class_ $ HH.ClassName "dropdown-content"]
      ( map (\x -> HH.a [ HP.href (uhq <> "" <> x) ] [ HH.text x ]) sts )
  ]
