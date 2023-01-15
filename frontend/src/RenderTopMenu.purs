module RenderTopMenu (
  renderTopMenu
) where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, isJust)
import Data.Map as M
import Data.Tuple (Tuple(..))
import Data.String (joinWith, stripPrefix)
import Data.String.Pattern (Pattern(..))

import Types (SortOrder (..), State, AlbumList(..),  Action(..))
import GetStuff ( _encodeURIComponent )

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
        , HE.onClick \_ -> ShowList $ AlbumList (Just "2023 Listened?&sortBy=Default&sortOrder=Desc")
        , HP.type_ HP.ButtonSubmit
        , HP.disabled state.loading
        ]
        [ HH.text "Listened" ]
      ]
    , HH.div [HP.class_ $ HH.ClassName "dropdown"]
      [ HH.button
        [ HP.class_ $ HH.ClassName "dropbtn1"
        , HE.onClick \_ -> ShowList $ AlbumList (Just "Discogs")
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

  uhq = state.menu.params.muhq
  sorts = state.menu.params.msorts
  sts = state.menu.params.msts
  listNames = state.menu.params.mlistNames
  locNames = state.menu.params.mlocNames

  sn = state.menu.sortName
  ln =  state.menu.ln
  ffs = state.menu.ffs
  sso = state.menu.sso


  fqs :: M.Map String Boolean
  fqs = M.fromFoldable
            <<< map (\t -> Tuple (fromMaybe t (stripPrefix (Pattern "-") t))
                                 (isNothing (stripPrefix (Pattern "-") t)))
            $ ffs

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

  renderFocus :: forall m. Array (H.ComponentHTML Action () m)
  renderFocus =
    let
        qry' :: Array String -> String -> String -- create the query url
        qry' ts n = uhq
                  <> n
                  <> "?"
                  <> ( _encodeURIComponent
                      <<< joinWith "&"
                      <<< map (\ (Tuple k v) -> k <> "=" <> v)
                      <<< map (\t -> Tuple "focus" t)
                      $ ts )
        lnk :: forall mm. M.Map String Boolean -> String -> H.ComponentHTML Action () mm
        lnk ts t = let
          nts :: Array String
          nts = map (\ (Tuple it ip) -> if ip then "#" <> it else "#-" <> it)
                    <<< M.toUnfoldable
                    <<< ttt $ ts
          p = isJust $ t `M.lookup` ts -- this tag was selected
          pp = fromMaybe false $ t `M.lookup` ts -- tag was True
          ttt:: M.Map String Boolean -> M.Map String Boolean
          ttt tts
                | p && pp   = M.insert t false tts
                | p         = M.delete t tts
                | otherwise = M.insert t true tts
          tc :: String
          tc
                | p && pp   = "focus-on"
                | p         = "focus-not"
                | otherwise = "focus"
        in HH.a [ HP.class_ $ HH.ClassName tc
                , HP.href (qry' nts ln)
                ]
                [ HH.text t ]
    in
    [
      HH.button [HP.class_ $ HH.ClassName "dropbtn"]
      [ HH.a [ HP.class_ $ HH.ClassName "dropbtn"
             , HP.href (qry' [ ] ln)
             ]
             [ HH.text $ if  ffs /= []
                          then "Focus (#" <> intercalate " #" ffs <> ")"
                          else "Focus "
             , HH.i [ HP.class_ $ HH.ClassName "fa fa-caret-down" ] []
             ]
      ]
      , HH.div [HP.class_ $ HH.ClassName "focus-content"]
          (map (lnk fqs) sts)
    ]


  renderButtonSort :: forall m. Array (H.ComponentHTML Action () m)
  renderButtonSort =
    [
      HH.button
      [ HP.class_ $ HH.ClassName "dropbtn" ]
      [ HH.text $ "Sort " <> if sn /= "Default"
                              then "by " <> sn <> " "
                              else ""
      ]
    , HH.div
      [ HP.class_ $ HH.ClassName "dropdown-content" ]
          -- ( map (\x -> HH.a [ HP.href (uhq <> ln <> "&sortBy=" <> x) ] [ HH.text x ]) sorts )
      ( map (\x -> HH.button [ HP.class_ $ HH.ClassName "xxx"
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
  renderButtonList =
    [
      HH.button
      [ HP.class_ $ HH.ClassName "dropbtn"]
      [ HH.text "List "
      , HH.i [ HP.class_ $ HH.ClassName "fa fa-caret-down" ] []
      ]
    , HH.div [HP.class_ $ HH.ClassName "dropdown-content"]
        ( map (\x -> HH.button [ HP.class_ $ HH.ClassName "xxx"
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
        ( map (\x -> HH.button [ HP.class_ $ HH.ClassName "dropdown"
                               , HE.onClick (\_ -> ShowList ( AlbumList (Just x) ))
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
