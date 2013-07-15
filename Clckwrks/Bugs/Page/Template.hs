{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Clckwrks.Bugs.Page.Template where

import Clckwrks
import Clckwrks.Bugs.Monad
import Clckwrks.Bugs.URL
import Clckwrks.Plugin
import Control.Monad.Reader
import Control.Monad.State
import Data.Text (Text)
import HSP.XML
import HSP.XMLGenerator
import Happstack.Server.HSP.HTML ()
import Web.Plugins.Core (Plugin(..), getPluginRouteFn, getTheme)

template :: ( EmbedAsChild BugsM headers
            , EmbedAsChild BugsM body
            ) =>
            Text
         -> headers
         -> body
         -> BugsM Response
template ttl hdrs bdy =
    do p <- plugins <$> get
       mTheme <- getTheme p
       (Just clckShowFn) <- getPluginRouteFn p (pluginName clckPlugin)
       case mTheme of
         Nothing      -> escape $ internalServerError $ toResponse $ ("No theme package is loaded." :: Text)
         (Just theme) ->
             do hdrXml <- fmap (map unClckChild) $ unXMLGenT $ asChild <%> <link rel="stylesheet" type="text/css" href=(BugsData "style.css") /> <% hdrs %></%>
                bdyXml <- fmap (map unClckChild) $ unXMLGenT $ asChild bdy
                fmap toResponse $ mapClckT f $ ClckT $ withRouteT (\f -> clckShowFn) $ unClckT $ unXMLGenT $ (_themeTemplate theme ttl hdrXml bdyXml)
    where
      f :: ServerPartT IO (a, ClckState) -> ReaderT BugsConfig (ServerPartT IO) (a, ClckState)
      f m = ReaderT $ \_ -> m

--       fmap toResponse $
--            themeTemplate p ttl <%> <link rel="stylesheet" type="text/css" href=(BugsData "style.css") /> <% hdrs %></%> bdy

--       let pageTemplate = undefined -- <- bugsPageTemplate <$> ask
--       undefined
{-
       fmap toResponse $ unXMLGenT $
            pageTemplate ttl <%> <link rel="stylesheet" type="text/css" href=(BugsData "style.css") /> <% hdrs %></%> bdy
-}
