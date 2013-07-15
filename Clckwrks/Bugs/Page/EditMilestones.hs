{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Clckwrks.Bugs.Page.EditMilestones where

import Control.Arrow        (first)
import Control.Monad.Reader (ask)
import Clckwrks
import Clckwrks.Bugs.Acid
import Clckwrks.Bugs.Monad
import Clckwrks.Bugs.Types
import Clckwrks.Bugs.URL
import Clckwrks.Bugs.Page.Template (template)
import Clckwrks.ProfileData.Acid (GetUserIdUsernames(..))
import Data.String (fromString)
import Data.Traversable (sequenceA)
import Data.Monoid (mempty)
import Data.Maybe  (fromJust, isJust)
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import qualified Data.Text.Lazy as TL
import HSP.XML
import HSP.XMLGenerator
import Text.Reform ( CommonFormError(..), Form, FormError(..), Proof(..), (++>)
                   , (<++), prove, transformEither, transform, view
                   )
import Text.Reform.Happstack
import Text.Reform.HSP.Text
import Text.Reform

editMilestones :: BugsURL -> BugsM Response
editMilestones here =
    do milestones <- query GetMilestones
       template (fromString "Edit Milestones") ()
         <%>
           <% reform (form here) (TL.pack "em") updateMilestones Nothing (editMilestonesForm milestones) %>
         </%>
    where
      updateMilestones :: ([Milestone], (Bool, Bool)) -> BugsM Response
      updateMilestones (_milestones, (False, True)) =
          do _mid <- update $ NewMilestone
             seeOtherURL here
      updateMilestones (milestones, (True, False)) =
          do update $ SetMilestones milestones
             seeOtherURL Timeline

-- |
--
-- FIXME: this can give odd results is the Milestone list changes
-- between the GET and POST requests. We need to use a different
-- pattern where the POST processing does not depend on the
-- [Milestone] parameter.
editMilestonesForm :: [Milestone] -> BugsForm ([Milestone], (Bool, Bool))
editMilestonesForm milestones =
  (divHorizontal $ fieldset $
    (,) <$> (sequenceA $ map editMilestoneForm milestones)
        <*> (    divFormActions $ (,)
             <$> (isJust <$> inputSubmit' (pack "update"))
             <*> (isJust <$> inputSubmit' (pack "add new milestone"))
            )
  )
    where
      divFormActions   = mapView (\xml -> [<div class="form-actions"><% xml %></div>])
      divHorizontal    = mapView (\xml -> [<div class="form-horizontal"><% xml %></div>])
      divControlGroup  = mapView (\xml -> [<div class="control-group"><% xml %></div>])
      divControls      = mapView (\xml -> [<div class="controls"><% xml %></div>])
      label' str       = (label str `setAttrs` [("class":="control-label") :: Attr TL.Text TL.Text])
      inputSubmit' str = inputSubmit str `setAttrs` [("class":="btn") :: Attr TL.Text TL.Text]

      editMilestoneForm ms@Milestone{..} =
          divControlGroup $
            label' ("#" ++ show (unMilestoneId milestoneId) ++" title:") ++>
                      (divControls ((\newTitle -> ms { milestoneTitle = newTitle }) <$> inputText milestoneTitle))

impure :: (Monoid view, Monad m) => m a -> Form m input error view () a
impure ma =
      Form $
        do i <- getFormId
           return (View $ const $ mempty, do a <- ma
                                             return $ Ok $ Proved { proofs    = ()
                                                                  , pos       = FormRange i i
                                                                  , unProved  = a
                                                                  })
