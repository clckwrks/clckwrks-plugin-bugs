{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Bugs.Page.SubmitBug where

import Control.Monad.Reader (ask)
import Clckwrks
import Clckwrks.Bugs.Acid
import Clckwrks.Bugs.Monad
import Clckwrks.Bugs.Types
import Clckwrks.Bugs.URL
import Clckwrks.Bugs.Page.Template (template)
import Clckwrks.Page.Types (Markup(..), PreProcessor(..))
import Data.String (fromString)
import Data.Monoid (mempty)
import Data.Maybe  (fromJust)
import Data.Time (UTCTime, getCurrentTime)
import Data.Text (Text, pack)
import qualified Data.Set as Set
import HSP
import Text.Reform ( CommonFormError(..), Form, FormError(..), Proof(..), (++>)
                   , (<++), prove, transformEither, transform, view)
import Text.Reform.Happstack
import Text.Reform.HSP.Text

import Text.Reform

submitBug :: BugsURL -> BugsM Response
submitBug here =
    do template (fromString "Submit a Report") ()
              <%>
               <h1>Submit Bug Report</h1>
               <% reform (form here) "sbr" addReport Nothing submitForm %>
              </%>
    where
      addReport :: Bug -> BugsM Response
      addReport bug =
          do ident <- update GenBugId
             update $ PutBug (bug { bugId = ident })
             seeOtherURL (ViewBug ident)

submitForm :: BugsForm Bug
submitForm =
  (divHorizontal $ fieldset $
    Bug <$> pure (BugId 0)
        <*> submittorIdForm
        <*> nowForm
        <*> pure New
        <*> pure Nothing
        <*> bugTitleForm
        <*> bugBodyForm
        <*> pure Set.empty
        <*> pure Nothing
        <*  (divFormActions $ inputSubmit' (pack "submit"))
  )
     where
      divFormActions   = mapView (\xml -> [<div class="form-actions"><% xml %></div>])
      divHorizontal    = mapView (\xml -> [<div class="form-horizontal"><% xml %></div>])
      divControlGroup  = mapView (\xml -> [<div class="control-group"><% xml %></div>])
      divControls      = mapView (\xml -> [<div class="controls"><% xml %></div>])
      inputSubmit' str = inputSubmit str `setAttrs` [("class":="btn")]
      label' str       = (label str `setAttrs` [("class":="control-label")])

      submittorIdForm :: BugsForm UserId
      submittorIdForm = impure (fromJust <$> getUserId)

      nowForm :: BugsForm UTCTime
      nowForm = impure (liftIO getCurrentTime)

      bugTitleForm :: BugsForm Text
      bugTitleForm =
          divControlGroup (label' (pack "Summary:") ++> (divControls $ inputText mempty `setAttrs` ["size" := "80", "class" := "input-xxlarge"]))

      bugBodyForm :: BugsForm Markup
      bugBodyForm =
          divControlGroup (label' (pack "Details:") ++> (divControls $ (\t -> Markup [HsColour, Markdown] t Untrusted) <$> (textarea 80 20 mempty `setAttrs` [("class" := "input-xxlarge")])))


impure :: (Monoid view, Monad m) => m a -> Form m input error view () a
impure ma =
      Form $
        do i <- getFormId
           return (View $ const $ mempty, do a <- ma
                                             return $ Ok $ Proved { proofs    = ()
                                                                  , pos       = FormRange i i
                                                                  , unProved  = a
                                                                  })


