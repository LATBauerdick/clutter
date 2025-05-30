-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module RenderAlbumView (renderAlbumView, renderAlbumText, renderAlbumJournal) where

import qualified Data.Text as T (replace, take)
import Data.Time
import qualified Lucid as L
import Relude
import RenderAlbumsView (renderBadges)
import RenderTopMenu (renderTopMenu)
import RenderUtil (renderHead)
import Types (Album (..), AppM, envGetEnvr)

renderAlbumView :: Maybe Album -> ZonedTime -> AppM (L.Html ())
renderAlbumView mAlbum now = do
  envr <- envGetEnvr
  env <- ask
  let h = L.html_ $ do
        renderHead "Album Page"
        L.body_ $ do
          albumBody
          renderTopMenu env envr "Discogs" []
  pure h
 where
  albumBody :: L.Html ()
  albumBody = do
    L.br_ []
    L.br_ []
    L.br_ []
    case mAlbum of
      Nothing ->
        L.div_ [L.class_ "login-message"] $ do
          L.p_ "Unknown Album, Sorry!"
          L.br_ []
          L.a_ [L.href_ "/albums/All"] "Please see all Albums"
      Just a ->
        case albumFormat a of
          "AppleMusic" ->
            L.div_ [L.class_ "login-message"] $ do
              L.p_ $ "Apple Music Album <" <> show (albumID a) <> ">."
              L.br_ []
              let qry = T.replace " " "+" $ albumArtist a <> "+-+" <> albumTitle a
              L.p_ $ do
                L.toHtml ("Search \"" <> albumTitle a <> "\" by " <> albumArtist a <> " on ")
                L.a_ [L.href_ $ "https://www.discogs.com/search/?q=" <> qry <> "&type=all"] "DISCOGS"
          "Tidal" ->
            L.div_ [L.class_ "login-message"] $ do
              L.p_ $ "Tidal Album <" <> show (albumID a) <> ">."
              L.br_ []
              let qry = T.replace " " "+" $ albumArtist a <> "+-+" <> albumTitle a
              L.p_ $ do
                L.toHtml ("Search \"" <> albumTitle a <> "\" by " <> albumArtist a <> " on ")
                L.a_ [L.href_ $ "https://www.discogs.com/search/?q=" <> qry <> "&type=all"] "DISCOGS"
          _ ->
            L.div_ [L.class_ "data-deskgap-drag"] $ do
              L.div_ [L.class_ "cover-container"] $ do
                L.a_ [L.href_ (albumURL a)] $ do
                  L.img_
                    [ L.src_ (albumCover a)
                    , L.alt_ "cover image"
                    , L.onerror_ "this.onerror=null;this.src='/no-cover.png';"
                    , L.class_ "cover-image"
                    ]
                renderBadges a
              L.p_ $ L.toHtml ("Title: " <> albumTitle a)
              L.p_ $ L.toHtml ("Artist: " <> albumArtist a)
              L.p_ $ L.toHtml ("Year: " <> albumReleased a)
              L.br_ []

              let qts = renderAlbumText a now
              L.div_ [L.class_ "quoteable"] $ do
                forM_
                  qts
                  ( \t -> do
                      L.samp_ $ L.toHtml t
                      L.br_ []
                  )

renderAlbumText :: Album -> ZonedTime -> [Text]
renderAlbumText a now = txts
 where
  ttl = T.replace ":" "_" . T.replace "/" "·" $ albumArtist a <> " – " <> albumTitle a
  dt :: Text
  dt = toText $ formatTime defaultTimeLocale "%y%m%d" now
  dtl = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" now
  fn = dt <> "-" <> ttl
  txts =
    [ fn
    , "---" :: Text
    , toText $ "date: " <> dtl
    , toText $ "title: " <> ttl
    , "---" :: Text
    , toText ("### " <> albumArtist a <> " – " <> albumTitle a)
    , toText ("[![](" <> albumCover a <> ")][1] ")
    , " "
    , -- icon with link to album page
      toText ("[1]: " <> albumURL a)
    , case albumAMusic a of
        Nothing -> ""
        Just amid ->
          if T.take 2 amid == "l."
            then toText ("[2]: " <> "https://music.apple.com/library/albums/" <> amid)
            else toText ("[2]: " <> "https://music.apple.com/us/album/" <> amid)
    , case albumTidal a of
        Nothing -> ""
        Just tid -> toText ("[3]: " <> "https://listen.tidal.com/album/" <> tid)
    , case albumAMusic a of
        Nothing -> ""
        Just _ -> toText ("[![[attachments/am-is.png]]][2]" :: Text)
    , case albumTidal a of
        Nothing -> ""
        Just _ -> toText ("[![[attachments/tidal-is.png]]][3]" :: Text)
    , -- embeded player for album
      case albumAMusic a of
        Nothing -> ""
        Just amid -> toText ("<iframe allow=\"autoplay *; encrypted-media *; fullscreen *\" frameborder=\"0\" height=\"450\" style=\"width:100%;max-width:660px;overflow:hidden;background:transparent;\" sandbox=\"allow-forms allow-popups allow-same-origin allow-scripts allow-storage-access-by-user-activation allow-top-navigation-by-user-activation\" src=\"https://embed.music.apple.com/us/album/turn-blue/" <> amid <> "\"></iframe>")
    , case albumTidal a of
        Nothing -> ""
        Just tid -> toText ("<div style=\"position: relative; padding-bottom: 100%; height: 0; overflow: hidden; max-width: 100%;\"><iframe src=\"https://embed.tidal.com/albums/" <> tid <> "?layout=gridify\" frameborder= \"0\" allowfullscreen style=\"position: absolute; top: 0; left: 0; width: 100%; height: 1px; min-height: 100%; margin: 0 auto;\"></iframe></div>")
    ]

renderAlbumJournal :: Album -> ZonedTime -> [Text]
renderAlbumJournal a now = txts
 where
  ttl = T.replace ":" "_" . T.replace "/" "·" $ albumArtist a <> " – " <> albumTitle a
  dt :: Text
  dt = toText $ formatTime defaultTimeLocale "%y%m%d" now
  dtl = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" now
  fn = dt <> "-" <> ttl
  txts =
    [ toText (albumCover a)
    , toText (albumArtist a <> " – " <> albumTitle a <> "<br>")
    , toText (albumURL a)
    -- , toText dtl
    ]
