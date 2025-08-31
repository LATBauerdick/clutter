module RenderTopMenu (
  renderTopMenu
) where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Map as M
import Data.Tuple (Tuple(..))

import Types (SortOrder (..), State, AlbumList(..),  Action(..))

renderTopMenu :: forall m'. State -> H.ComponentHTML Action () m'
renderTopMenu state =
  HH.div
    [ HP.id "navbar" ]
    [ HH.div [HP.class_ $ HH.ClassName "dropdown"] renderShow
    , HH.div [HP.class_ $ HH.ClassName "dropdown"] renderFocus
    , HH.div [HP.class_ $ HH.ClassName "dropdown"] renderButtonSort
    , HH.div [HP.class_ $ HH.ClassName "dropdown"] [renderButtonOrder]
    , HH.div [HP.class_ $ HH.ClassName "dropdown"]
      [ HH.button
        [ HP.class_ $ HH.ClassName "dropbtn1"
        , HE.onClick \_ -> ShowListSort (AlbumList (Just "2025 Listened")) "Default" Desc
        , HP.type_ HP.ButtonSubmit
        , HP.disabled state.loading
        ]
        [ HH.text "Listened" ]
      ]
    , HH.div [HP.class_ $ HH.ClassName "dropdown"]
      [ HH.button
        [ HP.class_ $ HH.ClassName "dropbtn1"
        , HE.onClick \_ -> SetFocus [ Tuple "played.never" true, Tuple "format.vinyl" true ]
        , HP.type_ HP.ButtonSubmit
        , HP.disabled state.loading
        ]
        [ HH.text "NPV" ]
      ]
    , HH.div [HP.class_ $ HH.ClassName "dropdown"]
      [ HH.button
        [ HP.class_ $ HH.ClassName "dropbtn1"
        , HE.onClick \_ -> UpdateDiscogs
        , HP.type_ HP.ButtonSubmit
        , HP.disabled state.loading
        ]
        [ HH.text "Discogs" ]
      ]
    , HH.div [HP.class_ $ HH.ClassName "dropdown"] renderButtonList
    , HH.div [HP.class_ $ HH.ClassName "dropdown"] renderButtonLocation
    , HH.div [HP.class_ $ HH.ClassName "dropdown"] renderButtonConfig
    ]
  where

  uhq = state.menu.params.muhq
  sorts = state.menu.params.msorts
  sts = state.menu.params.msts
  listNames = state.menu.params.mlistNames
  locNames = state.menu.params.mlocNames

  sn = state.menu.sortName
  ln =  state.menu.ln
  ffs =  state.menu.ffs
  sffs :: Array String
  sffs = map (\(Tuple a b) -> if b then a else "-" <> a) ffs
  sso = state.menu.sso

  renderShow :: forall m. Array ( H.ComponentHTML Action () m )
  renderShow = [
    HH.button [ HP.class_ $ HH.ClassName "dropbtn"
              , HE.onClick \_ -> ShowList ( AlbumList (Just ln) )
              ]
              [ HH.text $ "Showing " <> ln ]
  ]

  renderFocus :: forall m. Array (H.ComponentHTML Action () m)
  renderFocus =
    let
        lnk' :: forall mm. Array (Tuple String Boolean) -> String -> H.ComponentHTML Action () mm
        lnk' ts t = let
          mts :: M.Map String Boolean
          mts = M.fromFoldable ts
          p = isJust $ t `M.lookup` mts -- this tag was selected
          pp = fromMaybe false $ t `M.lookup` mts -- tag was True
          tc :: String
          tc
                | p && pp   = "focus-on"
                | p         = "focus-not"
                | otherwise = "focus"
        in HH.button  [ HP.class_ $ HH.ClassName tc
                      , HE.onClick \_ -> ToggleFocus t
                      ]
                      [ HH.text t ]
    in
    [
      HH.button [ HP.class_ $ HH.ClassName "dropbtn"
                , HE.onClick \_ -> SetFocus []
                ]
                [ HH.text $ if ffs == []
                             then "Focus "
                             else "Focus (#" <> intercalate " #" sffs <> ")"
                , HH.i [ HP.class_ $ HH.ClassName "fa fa-caret-down" ] []
                ]
      , HH.div [HP.class_ $ HH.ClassName "focus-content"]
          (map ( lnk' ffs ) sts)
    ]


  renderButtonSort :: forall m. Array (H.ComponentHTML Action () m)
  renderButtonSort =
    [
      HH.button
      [ HP.class_ $ HH.ClassName "dropbtn"
      , HE.onClick \_ -> SetSort "Default"
      ]
      [ HH.text $ "Sort " <> if sn /= "Default"
                              then "by " <> sn <> " "
                              else ""
      ]
    , HH.div
      [ HP.class_ $ HH.ClassName "dropdown-content" ]
      ( map (\x -> HH.button [ HP.class_ $ HH.ClassName "listbtn"
                             , HE.onClick (\_ -> SetSort x)
                             ]
                             [ HH.text x ])
            sorts )
    ]

  renderButtonOrder :: forall m. H.ComponentHTML Action () m
  renderButtonOrder =
    HH.button
      [ HP.class_ $ HH.ClassName "dropbtn-order"
      , HE.onClick \_ -> ToggleSortOrder
      , HP.type_ HP.ButtonSubmit
      , HP.disabled state.loading
      ]
      [ case sso of
               Asc  -> HH.i [ HP.class_ $ HH.ClassName "fa fa-chevron-circle-down" ] []
               Desc -> HH.i [ HP.class_ $ HH.ClassName "fa fa-chevron-circle-up" ] []
      ]

  renderButtonList :: forall m. Array ( H.ComponentHTML Action () m )
  renderButtonList = [
    HH.button [ HP.class_ $ HH.ClassName "dropbtn"]
      [ HH.text "List "
      , HH.i [ HP.class_ $ HH.ClassName "fa fa-caret-down" ] []
      ]
    , HH.div [HP.class_ $ HH.ClassName "dropdown-content"]
        ( map (\x -> HH.button [ HP.class_ $ HH.ClassName "listbtn"
                                , HE.onClick (\_ -> ShowList ( AlbumList (Just x) ))
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
        ( map (\x -> HH.button [ HP.class_ $ HH.ClassName "listbtn"
                               , HE.onClick (\_ -> ShowList ( AlbumList (Just x) ))
                               ]
                               [ HH.text x ])
                     locNames )
    ]

  renderButtonConfig :: forall w i. Array (HH.HTML w i)
  renderButtonConfig = [
    HH.button [HP.class_ $ HH.ClassName "dropbtn"]
    [ HH.text "... "
    , HH.i [ HP.class_ $ HH.ClassName "fa fa-caret-down" ] []
    ]
  , HH.div [HP.class_ $ HH.ClassName "dropdown-content"]
      ( map (\x -> HH.a [ HP.href (uhq <> "" <> x) ] [ HH.text x ]) sts )
  ]
