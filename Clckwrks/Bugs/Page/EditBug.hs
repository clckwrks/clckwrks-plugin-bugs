{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Bugs.Page.EditBug where

import Control.Arrow        (first)
import Control.Monad.Reader (ask)
import Clckwrks
import Clckwrks.Bugs.Acid
import Clckwrks.Bugs.Monad
import Clckwrks.Bugs.Types
import Clckwrks.Bugs.URL
import Clckwrks.Bugs.Page.Template (template)
import Clckwrks.Page.Types (Markup(..), PreProcessor(..))
import Clckwrks.ProfileData.Acid (GetUserIdUsernames(..))
import Data.Monoid (mempty)
import Data.Maybe  (fromJust)
import Data.String (fromString)
import Data.Time (UTCTime, getCurrentTime)
import Data.Text (Text, pack)
import qualified Data.Set as Set
import HSP
import Text.Reform ( CommonFormError(..), Form, FormError(..), Proof(..), (++>)
                   , (<++), prove, transformEither, transform, view
                   )
import Text.Reform.Happstack
import Text.Reform.HSP.Text

import Text.Reform

editBug :: BugsURL -> BugId -> BugsM Response
editBug here bid =
    do mBug <- query (GetBugById bid)
       case mBug of
         Nothing ->
             do notFound ()
                template (fromString "Bug not found.") ()
                         <h1>BugId Not Found: <% bid %></h1>
         (Just bug) ->
          do users      <- getUsers
             milestones <- query $ GetMilestones
             template (fromString "Edit Bug Report") ()
              <%>
               <h1>Edit Bug Report</h1>
               <% reform (form here) "sbr" updateReport Nothing (editBugForm users milestones bug) %>
              </%>
    where
      updateReport :: Bug -> BugsM Response
      updateReport bug =
          do update $ PutBug bug
             seeOtherURL (ViewBug bid)

      getUsers :: BugsM [(Maybe UserId, Text)]
      getUsers =
          ((Nothing, pack "Unassigned") :) . map (first Just) <$> query GetUserIdUsernames


editBugForm :: [(Maybe UserId, Text)] -> [Milestone] -> Bug -> BugsForm Bug
editBugForm users milestones bug@Bug{..} =
  (divHorizontal $ fieldset $
    Bug <$> pure bugId
        <*> pure bugSubmittor
        <*> pure bugSubmitted
        <*> bugStatusForm bugStatus
        <*> bugAssignedForm bugAssigned
        <*> bugTitleForm bugTitle
        <*> bugBodyForm bugBody
        <*> pure Set.empty
        <*> bugMilestoneForm bugMilestone
        <*  (divFormActions $ inputSubmit' (pack "update")))
    where
      divFormActions   = mapView (\xml -> [<div class="form-actions"><% xml %></div>])
      divHorizontal    = mapView (\xml -> [<div class="form-horizontal"><% xml %></div>])
      divControlGroup  = mapView (\xml -> [<div class="control-group"><% xml %></div>])
      divControls      = mapView (\xml -> [<div class="controls"><% xml %></div>])
      inputSubmit' str = inputSubmit str `setAttrs` [("class":="btn")]
      label' str       = (label str `setAttrs` [("class":="control-label")])


      bugStatusForm :: BugStatus -> BugsForm BugStatus
      bugStatusForm oldStatus =
          divControlGroup $ label' (pack "Status:") ++> (divControls $ select [(s, show s) | s <- [minBound .. maxBound]] (== oldStatus))

      bugAssignedForm :: Maybe UserId -> BugsForm (Maybe UserId)
      bugAssignedForm mUid =
          divControlGroup $ label' (pack "Assigned:") ++>
            (divControls $ select users (== mUid))

      bugTitleForm :: Text -> BugsForm Text
      bugTitleForm oldTitle =
          divControlGroup $ label' (pack "Summary:") ++> (divControls $ inputText oldTitle `setAttrs` ["size" := "80", "class" := "input-xxlarge"])

      bugBodyForm :: Markup -> BugsForm Markup
      bugBodyForm oldBody =
          divControlGroup $ label' (pack "Details:") ++> (divControls $ ((\t -> Markup [HsColour, Markdown] t Untrusted) <$> (textarea 80 20 (markup oldBody)  `setAttrs` [("class" := "input-xxlarge")])))

      bugMilestoneForm :: Maybe MilestoneId -> BugsForm (Maybe MilestoneId)
      bugMilestoneForm mMilestone =
          divControlGroup $ label' (pack "Milestone:") ++>
            (divControls $ select ((Nothing, pack "none") : [(Just $ milestoneId m, milestoneTitle m) | m <- milestones]) (== mMilestone))


impure :: (Monoid view, Monad m) => m a -> Form m input error view () a
impure ma =
      Form $
        do i <- getFormId
           return (View $ const $ mempty, do a <- ma
                                             return $ Ok $ Proved { proofs    = ()
                                                                  , pos       = FormRange i i
                                                                  , unProved  = a
                                                                  })
