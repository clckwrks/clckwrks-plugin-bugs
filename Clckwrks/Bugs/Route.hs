module Clckwrks.Bugs.Route where

import Control.Applicative          ((<$>))
import Control.Monad.Reader         (ask)
import Control.Monad.Trans          (liftIO)
import Clckwrks                     (Role(..), requiresRole_)
import Clckwrks.Bugs.Monad          (BugsM, BugsConfig(..))
import Clckwrks.Bugs.URL            (BugsURL(..), BugsAdminURL(..))
import Clckwrks.Bugs.Page.BugList   (bugList)
import Clckwrks.Bugs.Page.EditBug   (editBug)
import Clckwrks.Bugs.Page.EditMilestones (editMilestones)
import Clckwrks.Bugs.Page.SubmitBug (submitBug)
import Clckwrks.Bugs.Page.Timeline  (timeline)
import Clckwrks.Bugs.Page.ViewBug   (viewBug)
import qualified Data.Set           as Set
import Happstack.Server             (Response, notFound, toResponse, serveFile, guessContentTypeM, mimeTypes)
import Happstack.Server.FileServe.BuildingBlocks (isSafePath)
import Network.URI                  (unEscapeString)
import System.FilePath              ((</>), makeRelative, splitDirectories)
import Paths_clckwrks_plugin_bugs   (getDataDir)

checkAuth :: BugsURL
          -> BugsM BugsURL
checkAuth url =
    do showFn <- bugsClckURL <$> ask
       let requiresRole = requiresRole_ showFn
       case url of
         SubmitBug  {} -> requiresRole (Set.singleton Visitor) url
         ViewBug    {} -> return url
         SearchBugs {} -> return url
         BugsAdmin  {} -> requiresRole (Set.singleton Administrator) url
         BugsData   {} -> return url
         Timeline   {} -> return url
         BugList    {} -> return url

routeBugs :: BugsURL
          -> BugsM Response
routeBugs unsecureURL =
    do url <- checkAuth unsecureURL
       case url of
         (ViewBug bid) -> viewBug bid
         SubmitBug     -> submitBug url
         (BugsData fp')  ->
             do bugsDir <- liftIO getDataDir
                let fp'' = makeRelative "/" (unEscapeString fp')
                if not (isSafePath (splitDirectories fp''))
                  then notFound (toResponse ())
                  else serveFile (guessContentTypeM mimeTypes) (bugsDir </> "data" </> fp'')
         Timeline ->
             timeline
         BugList ->
             bugList
         BugsAdmin (EditBug bid) ->
             editBug url bid
         BugsAdmin EditMilestones ->
             editMilestones url
         SearchBugs ->
             notFound $ toResponse "not implemented yet."
