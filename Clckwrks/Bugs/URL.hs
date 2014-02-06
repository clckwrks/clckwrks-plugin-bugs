{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Clckwrks.Bugs.URL where

import Clckwrks.Bugs.Types (BugId(..))
import Data.Data           (Data, Typeable)
import Web.Routes.TH       (derivePathInfo)

data BugsAdminURL
    = EditBug BugId
    | EditMilestones
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(derivePathInfo ''BugsAdminURL)

data BugsURL
    = ViewBug BugId
    | SubmitBug
    | SearchBugs
    | BugsAdmin BugsAdminURL
    | BugsData FilePath
    | Timeline
    | BugList
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(derivePathInfo ''BugsURL)
