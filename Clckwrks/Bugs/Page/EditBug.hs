{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
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
import Data.Text (Text)
import qualified Data.Text      as T
import qualified Data.Text.Lazy as TL
import qualified Data.Set as Set
import HSP.XML
import HSP.XMLGenerator
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
               <% reform (form here) (TL.pack "sbr") updateReport Nothing (editBugForm users milestones bug) %>
              </%>
    where
      updateReport :: Bug -> BugsM Response
      updateReport bug =
          do update $ PutBug bug
             seeOtherURL (ViewBug bid)

      getUsers :: BugsM [(Maybe UserId, Text)]
      getUsers =
          ((Nothing, T.pack "Unassigned") :) . map (first Just) <$> query GetUserIdUsernames


editBugForm :: [(Maybe UserId, Text)] -> [Milestone] -> Bug -> BugsForm Bug
editBugForm users milestones bug@(Bug bugMeta@BugMeta{..} bugBody) =
  (divHorizontal $ fieldset $
    Bug <$> (BugMeta <$> pure bugId
                     <*> pure bugSubmitter
                     <*> pure bugSubmitted
                     <*> bugStatusForm bugStatus
                     <*> bugAssignedForm bugAssigned
                     <*> bugTitleForm bugTitle
                     <*> pure Set.empty
                     <*> bugMilestoneForm bugMilestone
            )
        <*> bugBodyForm bugBody
        <*  (divFormActions $ inputSubmit' (T.pack "update")))
    where
      divFormActions   = mapView (\xml -> [<div class="form-actions"><% xml %></div>])
      divHorizontal    = mapView (\xml -> [<div class="form-horizontal"><% xml %></div>])
      divControlGroup  = mapView (\xml -> [<div class="control-group"><% xml %></div>])
      divControls      = mapView (\xml -> [<div class="controls"><% xml %></div>])
      inputSubmit' str = inputSubmit str `setAttrs` [("class":="btn") :: Attr TL.Text TL.Text]
      label' str       = (label str `setAttrs` [("class":="control-label") :: Attr TL.Text TL.Text])


      bugStatusForm :: BugStatus -> BugsForm BugStatus
      bugStatusForm oldStatus =
          divControlGroup $ label' (T.pack "Status:") ++> (divControls $ select [(s, show s) | s <- [minBound .. maxBound]] (== oldStatus))

      bugAssignedForm :: Maybe UserId -> BugsForm (Maybe UserId)
      bugAssignedForm mUid =
          divControlGroup $ label' (T.pack "Assigned:") ++>
            (divControls $ select users (== mUid))

      bugTitleForm :: Text -> BugsForm Text
      bugTitleForm oldTitle =
          divControlGroup $ label' (T.pack "Summary:") ++> (divControls $ inputText oldTitle `setAttrs` ["size" := "80", "class" := "input-xxlarge" :: Attr TL.Text TL.Text])

      bugBodyForm :: Markup -> BugsForm Markup
      bugBodyForm oldBody =
          divControlGroup $ label' (T.pack "Details:") ++> (divControls $ ((\t -> Markup [HsColour, Markdown] t Untrusted) <$> (textarea 80 20 (markup oldBody)  `setAttrs` [("class" := "input-xxlarge") :: Attr TL.Text TL.Text])))

      bugMilestoneForm :: Maybe MilestoneId -> BugsForm (Maybe MilestoneId)
      bugMilestoneForm mMilestone =
          divControlGroup $ label' (T.pack "Milestone:") ++>
            (divControls $ select ((Nothing, T.pack "none") : [(Just $ milestoneId m, milestoneTitle m) | m <- milestones]) (== mMilestone))


impure :: (Monoid view, Monad m) => m a -> Form m input error view () a
impure ma =
      Form $
        do i <- getFormId
           return (View $ const $ mempty, do a <- ma
                                             return $ Ok $ Proved { proofs    = ()
                                                                  , pos       = FormRange i i
                                                                  , unProved  = a
                                                                  })
