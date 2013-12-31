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
       (Just clckShowFn) <- getPluginRouteFn p (pluginName clckPlugin)
       hdrXml <- fmap (map unClckChild) $ unXMLGenT $ asChild <%> <link rel="stylesheet" type="text/css" href=(BugsData "style.css") /> <% hdrs %></%>
       bdyXml <- fmap (map unClckChild) $ unXMLGenT $ asChild bdy
       fmap toResponse $ mapClckT f $ ClckT $ withRouteT (\f -> clckShowFn) $ unClckT $ (themeTemplate p (ThemeStyleId 0) ttl hdrXml bdyXml)
    where
      f :: ServerPartT IO (a, ClckState) -> ReaderT BugsConfig (ServerPartT IO) (a, ClckState)
      f m = ReaderT $ \_ -> m
