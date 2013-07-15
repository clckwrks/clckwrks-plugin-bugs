{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Clckwrks.Bugs.Page.Timeline where

import Clckwrks
import Clckwrks.Bugs.Acid
import Clckwrks.Bugs.Monad
import Clckwrks.Bugs.Types
import Clckwrks.Bugs.URL
import Clckwrks.Bugs.Page.Template (template)
import qualified Data.IxSet as IxSet
import Data.List (find)
import Data.String (fromString)
import qualified Data.Text.Lazy as Text
import Numeric (showFFloat)
import HSP.XML
import HSP.XMLGenerator

timeline :: BugsM Response
timeline =
    template (fromString "Timeline") ()
        <%>
          <h1>Timeline</h1>
          <% timelineWidget %>
        </%>

timelineWidget :: XMLGenT BugsM XML
timelineWidget =
    do ms     <- query GetMilestones
       ixBugs <- query $ BugsForMilestones (map milestoneId ms)
       <div class="timeline">
        <% mapM (showMilestone ms) (IxSet.groupBy ixBugs) %>
       </div>

showMilestone :: [Milestone] -> (MilestoneId, [Bug]) -> XMLGenT BugsM [ChildType BugsM]
showMilestone ms (mid, bugs) =
    case find (\m -> milestoneId m == mid) ms of
      Nothing -> <%>internal error: showMilestone - not found <% show mid %></%>
      (Just m) ->
       do completed <- query (MilestoneCompletion mid)
          <%>
           <h2><% milestoneTitle m %></h2>
           <% maybe (return $ cdata $ fromStringLit "") meter completed %>
           <table>
            <thead>
             <tr>
              <th>BugId</th>
              <th>Summary</th>
             </tr>
            </thead>
            <tbody>
             <% mapM showBugSummary bugs %>
            </tbody>
           </table>
          </%>

showBugSummary :: Bug -> XMLGenT BugsM XML
showBugSummary Bug{..} =
    <tr [Text.pack "class" := (if (bugStatus == New) || (bugStatus == Accepted) then (Text.pack "bug-summary-open") else (Text.pack "bug-summary-closed"))] >
     <td><a href=(ViewBug bugId)><% bugId %></a></td>
     <td><% bugTitle %></td>
    </tr>

meter :: (Real a)
      => a -- ^ a number between 0 and 1 indicating percentage complete
      -> XMLGenT BugsM XML
meter completed =
    <div class="meter">
      <span style=(Text.pack $ "width: " ++ (showFFloat (Just 3) ((fromRational (toRational completed)) * 100) "%"))></span>
    </div>
