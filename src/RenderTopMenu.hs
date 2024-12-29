{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}

module RenderTopMenu ( renderTopMenu )
where

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import qualified Lucid as L
import qualified Data.Text as T ( find, stripPrefix, intercalate )
import Relude

import Types ( Env (..), EnvR (..), SortOrder (..), pLocList )

import RenderUtil ( formUrlEncodeQuery )

renderTopMenu :: Env -> EnvR -> Text -> [Text] -> L.Html ()
renderTopMenu env envr ln fs = L.div_ [L.id_ "navbar"] $ do
  let ffs :: [Text]
      ffs = mapMaybe (T.stripPrefix "#") fs
  let fqs :: Map Text Bool
      fqs = M.fromList
                . map (\t ->  (fromMaybe t (T.stripPrefix "-" t)
                              , isNothing (T.stripPrefix "-" t)))
                $ ffs
  let sts :: [Text] -- sorted tags
      sts = filter (isJust . T.find ('.' ==)) (M.keys (tags envr))
          <> filter (isNothing . T.find ('.' ==)) (M.keys (tags envr))
  let uhq :: Text; uhq = url env <> "albums/"
      qry' :: [Text] -> Text -> Text -- create the query url
      qry' ts n = uhq
                <> n
                <> "?"
                <> ( decodeUtf8 . formUrlEncodeQuery
                    . map (\t -> ("focus", toString t))
                    $ ts )

  let
      renderShow = do
        L.button_ [L.class_ "dropbtn"] $ do
          L.a_  [L.class_ "dropbtn", L.href_ (qry' fs ln)] $ L.toHtml $ "Showing " <> ln

      renderButtonList = do
        L.button_ [L.class_ "dropbtn"] $ do
          "List "
          L.i_ [ L.class_ "fa fa-caret-down" ] ""
        L.div_ [L.class_ "dropdown-content"] $ do
          F.traverse_ (\x -> L.a_ [L.href_ (qry' fs x)] $ do L.toHtml x)
            . filter (not . pLocList) $ M.keys (listNames envr)

      renderButtonLocation = do
        L.button_ [L.class_ "dropbtn"] $ do
          "Location "
          L.i_ [ L.class_ "fa fa-caret-down" ] ""
        L.div_ [L.class_ "dropdown-content"] $ do
          F.traverse_ (\x -> L.a_ [L.href_ (qry' fs x)] $ do L.toHtml x)
            . filter pLocList $ M.keys (listNames envr)

      renderButtonTags = do
        L.button_ [L.class_ "dropbtn"] $ do
          "Tags "
          L.i_ [ L.class_ "fa fa-caret-down" ] ""
        L.div_ [L.class_ "dropdown-content"] $ do
          F.traverse_ (\x -> L.a_ [L.href_ (qry' fs ("#" <> x))] $ do L.toHtml x)
            sts -- M.keys (tags envr)

      renderButtonSort = do
        let addLink :: Text -> Text -> L.Html ()
            addLink t0 t1 =
              L.a_ [L.href_ (t0 <> t1)] $ do
                L.toHtml t1

        L.button_ [L.class_ "dropbtn"] $ do
          L.toHtml $ "Sort " <> if sortName envr /= "Default"
                      then "by " <> sortName envr <> " "
                      else ""
        L.div_ [L.class_ "dropdown-content"] $ do
          F.traverse_ (addLink ((qry' fs ln) <> "&sortBy=")) (sorts env)

      renderButtonOrder = do
        L.button_ [L.class_ "dropbtn-order"] $ do
          let sso = case sortOrder envr of
                      Asc  -> Desc
                      Desc -> Asc
          L.a_  [L.href_ ((qry' fs ln) <> "&sortOrder=" <> show sso)] $
              case sortOrder envr of
                Asc ->  L.i_ [ L.class_ "fa fa-chevron-circle-down" ] ""
                Desc -> L.i_ [ L.class_ "fa fa-chevron-circle-up" ] ""

      renderFocus = do
        let lnk :: Map Text Bool -> Text -> L.Html ()
            lnk ts t = do
              let nts = map (\(it, ip) -> if ip then "#" <> it else "#-" <> it)
                        . M.toList
                        . ttt $ ts
                        -- . M.insertWith xor t True $ ts
                  p = isJust $ t `M.lookup` ts -- this tag was selected
                  pp = fromMaybe False $ t `M.lookup` ts -- tag was True
                  ttt:: Map Text Bool -> Map Text Bool; ttt tts
                    | p && pp   = M.insert t False tts
                    | p         = M.delete t tts
                    | otherwise = M.insert t True tts
                  tc :: Text; tc
                    | p && pp   = "focus-on"
                    | p         = "focus-not"
                    | otherwise = "focus"
              L.a_  [ L.class_ tc
                    , L.href_ (qry' nts ln)] $ do
                L.toHtml t

        L.button_ [L.class_ "dropbtn"] $ do
          L.a_  [L.class_ "dropbtn", L.href_ (qry' [] ln) ] $ do --L.toHtml $ "Showing " <> ln
            L.toHtml $ if  ffs /= []
                        then "Focus (#" <> T.intercalate " #" ffs <> ")"
                        else "Focus "
            L.i_ [ L.class_ "fa fa-caret-down" ] ""

        L.div_ [L.class_ "focus-content"] $ do
          F.traverse_ (lnk fqs) sts

  L.div_ [L.class_ "dropdown"] renderShow
  L.div_ [L.class_ "dropdown"] renderFocus
  L.div_ [L.class_ "dropdown"] renderButtonSort
  L.div_ [L.class_ "dropdown"] renderButtonOrder
  L.a_   [L.class_ "active"
          , L.href_ (uhq <> "2024 Listened?&sortBy=Default&sortOrder=Desc")] "Listened"
  L.a_   [L.class_ "active"
          , L.href_ (uhq <> "Discogs?focus=%23format.vinyl&focus=%23played.never")] "NPV"
  L.a_   [L.class_ "active", L.href_ (uhq <> "Discogs")] "Discogs"
  L.div_ [L.class_ "dropdown"] renderButtonList
  L.div_ [L.class_ "dropdown"] renderButtonLocation
  L.div_ [L.class_ "dropdown"] renderButtonTags
