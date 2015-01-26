{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Clckwrks.Bugs.Page.BugList where

import Clckwrks
import Clckwrks.Bugs.Acid
import Clckwrks.Bugs.Monad
import Clckwrks.Bugs.Types
import Clckwrks.Bugs.URL
import Clckwrks.Bugs.Page.Template (template)
import Clckwrks.ProfileData.Acid (GetProfileData(..))
import qualified Data.IxSet as IxSet
import Data.List (find)
import Data.String (fromString)
import qualified Data.Text.Lazy as Text
import Numeric (showFFloat)
import HSP.XML
import HSP.XMLGenerator

bugList :: BugsM Response
bugList =
    template (fromString "Bug List") ()
        <%>
          <h1>Bug List</h1>
          <% bugListWidget %>
        </%>

bugListWidget :: XMLGenT BugsM XML
bugListWidget =
    do bugs <- query (AllBugMeta (Desc SortByBugId))
       <div class="bug-list">
        <table class="table">
         <thead>
          <tr>
           <th>#</th>
           <th>Title</th>
           <th>Status</th>
           <th>Assigned</th>
          </tr>
         </thead>
         <tbody>
          <% map showBugMeta bugs %>
         </tbody>
        </table>
       </div>

showBugMeta :: BugMeta -> XMLGenT BugsM XML
showBugMeta BugMeta{..} =
    <tr [Text.pack "class" := (if (bugStatus == New) || (bugStatus == Accepted) then (Text.pack "bug-summary-open") else (Text.pack "bug-summary-closed"))] >
     <td><a href=(ViewBug bugId)><% bugId %></a></td>
     <td><% bugTitle %></td>
     <td><% show bugStatus %></td>
     <td><% case bugAssigned of
              Nothing -> <% () %>
              (Just u) -> do pd <- query (GetProfileData u)
                             <% username pd %>
          %>
     </td>
    </tr>
