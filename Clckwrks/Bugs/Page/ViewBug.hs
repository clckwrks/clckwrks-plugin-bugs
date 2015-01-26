{-# LANGUAGE FlexibleContexts, RecordWildCards, QuasiQuotes #-}
module Clckwrks.Bugs.Page.ViewBug where

import Clckwrks
import Clckwrks.Bugs.Acid
import Clckwrks.Bugs.Monad
import Clckwrks.Bugs.Types
import Clckwrks.Bugs.URL
import Clckwrks.Bugs.Page.Template (template)
import Clckwrks.Page.Monad         (markupToContent)
import Clckwrks.ProfileData.Acid
import Control.Monad.State
import Data.Maybe (fromMaybe, maybe)
import Data.Set   (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text  (pack)
import HSP.XML
import HSP.XMLGenerator
import Language.Haskell.HSX.QQ (hsx)

viewBug :: BugId -> BugsM Response
viewBug bid =
    do mBug <- query (GetBugById bid)
       case mBug of
         Nothing -> do notFound ()
                       template (fromString "bug not found.") ()
                        [hsx| <p>Could not find Bug #<% show $ unBugId bid %></p> |]
         (Just bug) -> bugHtml bug

bugHtml :: Bug -> BugsM Response
bugHtml (Bug BugMeta{..} bugBody) =
    do submitter       <- query (GetUsername bugSubmitter)
       milestoneTxt <-
           case bugMilestone of
             Nothing  -> return (pack "none")
             Just mid ->
                 fromMaybe (pack $ show mid) <$> query (GetMilestoneTitle mid)
       bugBodyMarkup <- markupToContent bugBody
       template (fromString $ "Bug #" ++ (show $ unBugId bugId)) () [hsx|
         <%>
           <h1>View Bug</h1>
           <dl id="view-bug">
            <dt>Bug #:</dt>       <dd><% show $ unBugId bugId %></dd>
            <dt>Submitted By:</dt><dd><% submitter %></dd>
            <dt>Submitted:</dt>   <dd><% bugSubmitted %></dd>
            <dt>Status:</dt>      <dd><% show bugStatus %></dd>
            <dt>Milestone:</dt>   <dd><% milestoneTxt %></dd>
            <dt>Title:</dt>       <dd><% bugTitle %></dd>
            <dt>Body:</dt>        <dd><% bugBodyMarkup %></dd>
            <% whenHasRole (Set.singleton Administrator) <a href=(BugsAdmin (EditBug bugId))>edit</a> %>
           </dl>
         </%> |]

whenHasRole :: (Happstack m) =>
               Set Role
            -> XMLGenT (ClckT url m) XML
            -> XMLGenT (ClckT url m) XML
whenHasRole role xml =
    do muid <- lift getUserId
       case muid of
         (Just uid) ->
             do b <- query (HasRole uid role)
                if b
                  then xml
                  else return $ cdata $ fromStringLit ""
         _ -> return $ cdata $ fromStringLit ""
