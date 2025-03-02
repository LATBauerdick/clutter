-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Render (renderApp, renderHead, renderAlbumView, renderAlbumsView, renderAlbumText, renderAlbumJournal) where

import RenderAlbumView (renderAlbumJournal, renderAlbumText, renderAlbumView)
import RenderAlbumsView (renderAlbumsView)
import RenderUtil (renderApp, renderHead)
