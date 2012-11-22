{-# LANGUAGE RecordWildCards, FlexibleContexts, OverloadedStrings #-}
module Clckwrks.Bugs.Plugin where

import Clckwrks
import Clckwrks.Plugin
import Clckwrks.Bugs.URL
import Clckwrks.Bugs.Acid
import Clckwrks.Bugs.PreProcess    (bugsCmd)
import Clckwrks.Bugs.Monad
import Clckwrks.Bugs.Route
import Data.Acid.Local
import Data.Text           (Text)
import Data.Maybe          (fromMaybe)
import System.FilePath     ((</>))
import Web.Plugin.Core
import Paths_clckwrks_plugin_bugs (getDataDir)

bugsHandler :: (BugsURL -> [(Text, Maybe Text)] -> Text)
            -> BugsConfig
            -> ClckPlugins
            -> [Text]
            -> ClckT ClckURL (ServerPartT IO) Response
bugsHandler showBugsURL bugsConfig plugins paths =
    case parseSegments fromPathSegments paths of
      (Left e)  -> notFound $ toResponse (show e)
      (Right u) ->
          ClckT $ withRouteT flattenURL $ unClckT $ runBugsT bugsConfig $ routeBugs u
    where
      flattenURL ::   ((url' -> [(Text, Maybe Text)] -> Text) -> (BugsURL -> [(Text, Maybe Text)] -> Text))
      flattenURL _ u p = showBugsURL u p

bugsInit :: ClckPlugins
         -> IO (Maybe Text)
bugsInit plugins =
    do (Just bugsShowFn) <- getPluginRouteFn plugins (pluginName bugsPlugin)
       (Just clckShowFn) <- getPluginRouteFn plugins (pluginName clckPlugin)
       mTopDir <- clckTopDir <$> getConfig plugins
       let basePath = maybe "_state" (\td -> td </> "_state") mTopDir -- FIXME
       acid <- openLocalStateFrom (basePath </> "bugs") initialBugsState
       addCleanup plugins Always (createCheckpointAndClose acid)
       let bugsConfig = BugsConfig { bugsDirectory    = "bugs-dir"
                                   , bugsState        = acid
                                   , bugsClckURL      = clckShowFn
                                   }
       addPreProc plugins (pluginName bugsPlugin) (bugsCmd bugsShowFn)
       addHandler plugins (pluginName bugsPlugin) (bugsHandler bugsShowFn bugsConfig)
       return Nothing

bugsPlugin :: Plugin BugsURL Theme (ClckT ClckURL (ServerPartT IO) Response) (ClckT ClckURL IO ()) ClckwrksConfig (ClckT ClckURL IO)
bugsPlugin = Plugin
    { pluginName       = "bugs"
    , pluginInit       = bugsInit
    , pluginDepends    = []
    , pluginToPathInfo = toPathInfo
    , pluginPostHook   =
         do dd <- liftIO getDataDir
            addPluginPath (pluginName bugsPlugin) dd
    }

plugin :: ClckPlugins -- ^ plugins
       -> Text        -- ^ baseURI
       -> IO (Maybe Text)
plugin plugins baseURI =
    initPlugin plugins baseURI bugsPlugin
