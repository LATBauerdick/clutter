module AlbumComponent (
  aComponent
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), fromRight)
import Effect.Class.Console as Console

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
-- import Halogen.HTML.CSS as CSS
-- import CSS (backgroundColor, fontSize, px)

import Effect.Aff.Class (class MonadAff)
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Data.DateTime (DateTime)
import Data.Formatter.DateTime (formatDateTime)
import Data.String (take) as S
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))

import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson)

import Types (Album, AlbumJ, MenuState, SortOrder(..))
import GetStuff (getUrl, getNow)
import RenderTopMenu (renderTopMenu)

noAlbum :: forall w i. HH.HTML w i
noAlbum =
  HH.div
    [ HP.id "root" ]
    [
      HH.h1 [ ] [ HH.text "This is the Clutter App!" ]
    , HH.div
      [ HP.class_ $ HH.ClassName "login-message" ]
      [ HH.span_
        [ HH.text "Unknown album, sorry! Please see "
        , HH.a
          [HP.href "http://localhost:8080/albums/All"]
          [HH.text "all Albums" ]
        ]
      ]
    ]

albumElement :: forall w i. Album -> DateTime -> HH.HTML w i
albumElement a now =
  case a.albumFormat of
            "AppleMusic" -> appleMusicView
            "Tidal" -> tidalView
            _ -> discogsView
  where
  appleMusicView = discogsView
  tidalView = discogsView

  ttl = replaceAll (Pattern ":") (Replacement "_") <<< replaceAll (Pattern "/") (Replacement "·") $ a.albumArtist <> " - " <> a.albumTitle
  dt = fromRight "???????" <<< formatDateTime "YYMMDD" $ now -- "2022-10-19T20:01"
  dtl = fromRight "???????" <<< formatDateTime "YYYY-MM-DDTHH:mm" $ now -- "2022-10-19T20:01"
  discogsView =
    HH.div
      [ HP.class_ $ HH.ClassName "data-deskgap-drag" ]
      [ HH.div
        [ HP.class_ $ HH.ClassName "cover-container" ]
        [ HH.p_
          [ HH.text "Great album! Have a look on Clutter "
          , HH.a
            [ HP.href $ "http://localhost:8080/album/" <> show a.albumID ]
            [ HH.text "here" ]
          ]
        , HH.a
          [ HP.href a.albumURL]
          [ HH.img [ HP.src a.albumCover
                   , HP.alt "cover image"
                   --, HP.onerror "this.onerror=null;this.src='/no-cover.png';"
                   , HP.class_ $ HH.ClassName "cover-image"
                   ]
          ]
        ]
      , HH.p_ [HH.text ("Title: "  <> a.albumTitle)]
      , HH.p_ [HH.text ("Artist: " <> a.albumArtist)]
      , HH.p_ [HH.text ("Year: "   <> a.albumReleased)]
      , HH.br_
      , HH.div
          [ HP.class_ ( HH.ClassName "quoteable" )
          -- , CSS.style do
          --     fontSize $ px 20.0
          --     backgroundColor orange
          ]
          ([ HH.samp_ [ HH.text $ dt <> "-" <> ttl ]
          , HH.br_
          , HH.samp_ [ HH.text "---" ]
          , HH.br_
          , HH.samp_ [ HH.text $ "date: " <> dtl ]
          , HH.br_
          , HH.samp_ [ HH.text ("title: " <> ttl) ]
          , HH.br_
          , HH.samp_ [ HH.text "---" ]
          , HH.br_
          , HH.samp_ [ HH.text ("### " <> a.albumArtist <> " – " <> a.albumTitle) ]
          , HH.br_
          , HH.samp_ [ HH.text ("[![](" <> a.albumCover <> ")][1] ") ]
          , HH.br_
          -- reference-style link to album page
          , HH.br_
          , HH.samp_ [ HH.text ("[1]: " <> a.albumURL) ]
          ]
          <> case a.albumAMusic of
                  Nothing -> []
                  Just amid ->  [ HH.br_
                                , if S.take 2 amid == "l."
                                    then HH.samp_ [ HH.text ("[2]: " <> "https://music.apple.com/library/albums/" <> amid) ]
                                    else HH.samp_ [ HH.text ("[2]: " <> "https://music.apple.com/us/album/" <> amid) ]
                                ]
          <> case a.albumTidal of
                  Nothing -> []
                  Just tid ->  [ HH.br_
                               , HH.samp_ [ HH.text ("[3]: " <> "https://listen.tidal.com/album/" <> tid) ]
                               ]
          -- icon with link to album page
          <> [ HH.br_
             , HH.br_
             ]
          <> case a.albumAMusic of
                  Nothing -> []
                  Just _ -> [ HH.samp_ [ HH.text "[![[attachments/am-is.png]]][2]"  ]
                            ]
          <> case a.albumTidal of
                  Nothing -> []
                  Just _ -> [ HH.samp_ [ HH.text "[![[attachments/tidal-is.png]]][3]" ] ]
          -- embeded player for album
          <> [ HH.br_ ]
          <> case a.albumAMusic of
                  Nothing -> []
                  Just amid -> [ HH.br_
                               , HH.samp_ [ HH.text $ "<iframe allow=\"autoplay *; encrypted-media *; fullscreen *\" frameborder=\"0\" height=\"450\" style=\"width:100%;max-width:660px;overflow:hidden;background:transparent;\" sandbox=\"allow-forms allow-popups allow-same-origin allow-scripts allow-storage-access-by-user-activation allow-top-navigation-by-user-activation\" src=\"https://embed.music.apple.com/us/album/turn-blue/" <> amid <> "\"></iframe>" ]
                               ]
          <> case a.albumTidal of
                  Nothing -> []
                  Just tid -> [ HH.br_
                              , HH.samp_ [ HH.text $ "<div style=\"position: relative; padding-bottom: 100%; height: 0; overflow: hidden; max-width: 100%;\"><iframe src=\"https://embed.tidal.com/albums/" <> tid <> "?layout=gridify\" frameborder= \"0\" allowfullscreen style=\"position: absolute; top: 0; left: 0; width: 100%; height: 1px; min-height: 100%; margin: 0 auto;\"></iframe></div>" ]
                  ]

          )
      , HH.iframe
        [ HP.src ("http://localhost:8080/album/" <> show a.albumID)
        , HP.title "iframe_a"
        , HP.style "height:600px;width:100%;"
        -- , frameborder "0"
        -- , allow "autoplay *; encrypted-media *; fullscreen *"
        ]
      ]
-- allow :: forall r i. String -> HH.IProp ( allow :: String | r ) i
-- allow = HH.prop (HH.PropName "allow")
-- frameborder :: forall r i. String -> HH.IProp ( frameborder :: String | r ) i
-- frameborder = HH.prop (HH.PropName "frameborder")

albumView :: forall i w. Maybe Album -> DateTime -> HH.HTML w i
albumView am now = case am of
                   Just a -> HH.div_ [ albumElement a now ]
                   Nothing -> HH.div_ [ noAlbum ]

type State =  { album :: Maybe Album
              , loading :: Boolean
              , albumID :: String
              , now :: DateTime
              , result :: Maybe String
              , menu :: MenuState
              }
data Action = Increment | Decrement | SetAlbumID String | MakeRequest Event

aComponent :: forall query input output m. MonadAff m => Maybe Album -> DateTime -> H.Component query input output m
aComponent am now =
  H.mkComponent
  { initialState: initialState am now
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

initialState :: forall input. Maybe Album -> DateTime -> input -> State
initialState am now _ = { album: am, loading: false, albumID: "659642", now: now, result: Nothing, menu : initialMenuState }

initialMenuState :: MenuState
initialMenuState =
  { uhq : "localhost:8080/albums/"
  , ln : "Discogs" -- list
  , ffs : [ "pop" ]
  , sorts : [ "Added", "Artist", "Default", "Title" ]
  , sortName : "Default"
  , sso : Asc
  , sts : [ "folder.cd", "folder.pop", "genre.classical", "genre.opera", "rated.*****" ] -- sorted tags
  , listNames : [ "2022 Listened", "All", "Apple Music",  "Discogs", "Pop", "Tidal" ]
  , locNames : [ "Cube A0", "Cube B0 Pop", "Cube E0 Incoming", "Cube E1 Incoming", "Shelf A1 Opera" ]
  }


render :: forall m. State -> H.ComponentHTML Action () m
render state = do
  HH.div_
    [
      renderTopMenu state.menu
    , HH.h1_ [ HH.text "This it the Clutter App!" ]
    , HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
    , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
    , HH.form
      [ HE.onSubmit \ev -> MakeRequest ev ]
      [ HH.h3_ [ HH.text "Look up Album ID" ]
      , HH.label_
          [ HH.div_ [ HH.text "Enter album id:" ]
          , HH.input
              [ HP.value state.albumID
              , HE.onValueInput \str -> SetAlbumID str
              ]
          ]
      , HH.button
          [ HP.disabled state.loading
          , HP.type_ HP.ButtonSubmit
          ]
          [ HH.text "Fetch info" ]
      , HH.p_
          [ HH.text $ if state.loading then "Working..." else "" ]
      , HH.div_
          case state.result of
            Nothing -> []
            Just res ->
              [ HH.h2_
                  [ HH.text "Response:" ]
              , HH.pre_
                  [ HH.code_ [ HH.text res ] ]
              ]
      ]
    , albumView state.album state.now
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Decrement -> do
    H.modify_ _ { album = Nothing }
  Increment ->
    H.modify_ _ { album = Nothing }
  SetAlbumID albumID -> do
    now <- H.liftAff getNow
    H.liftAff $ Console.logShow now
    H.modify_ _ { albumID = albumID, now = now, result = Nothing }
  MakeRequest event -> do
    H.liftEffect $ Event.preventDefault event
    albumID <- H.gets _.albumID
    now <- H.liftAff getNow
    H.modify_ _ { loading = true }
    r <- H.liftAff $ getUrl ("http://localhost:8080/albumj/" <> albumID)
    H.liftAff $ Console.logShow ((decodeJson =<< parseJson r) :: Either JsonDecodeError AlbumJ)
    let aje :: Either JsonDecodeError AlbumJ
        aje = (decodeJson =<< parseJson r) -- :: Either JsonDecodeError AlbumJ
    let am = case aje of
                      Right { aid: _, album: a } -> Just a
                      Left _ -> Nothing

    H.modify_ _ { album = am, loading = false, result = Just r, now = now}
    -- response <- H.liftAff $ AX.get ResponseFormat.string ("https://api.github.com/users/" <> albumID)
    -- H.modify_ _ { loading = false, result = map _.body (hush response) }
