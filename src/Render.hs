-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Render (renderApp, renderHead, renderAlbumView, renderAlbumsView, renderAlbumText) where

import RenderAlbumView (renderAlbumText, renderAlbumView)
import RenderAlbumsView (renderAlbumsView)
import RenderUtil (renderApp, renderHead)

