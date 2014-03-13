{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell, TypeFamilies #-}
module Clckwrks.Bugs.Types where

import Clckwrks
import Clckwrks.Page.Types (Markup(..), PreProcessor(..))
import Data.Data           (Data, Typeable)
import Data.IxSet          (Indexable(..), ixSet, ixFun)
import Data.Maybe          (maybeToList)
import Data.SafeCopy       (SafeCopy(..), Migrate(..), base, contain, deriveSafeCopy, extension, safeGet, safePut)
import qualified Data.Serialize as S
import Data.Text           (Text)
import qualified Data.Text.Encoding as T
import Data.Time           (UTCTime)
import Data.Set            (Set)
import Web.Routes          (PathInfo(..))

newtype BugId = BugId { unBugId :: Integer }
    deriving (Eq, Ord, Read, Show, Data, Typeable, PathInfo)

instance SafeCopy BugId where
    getCopy = contain $ fmap BugId S.get
    putCopy = contain . S.put . unBugId
    errorTypeName _ = "BugId"

newtype BugTag = BugTag { tagText :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, PathInfo)

instance SafeCopy BugTag where
    kind = base
    getCopy = contain $ (BugTag . T.decodeUtf8) <$> safeGet
    putCopy = contain . safePut . T.encodeUtf8 . tagText
    errorTypeName _ = "BugTag"

newtype MilestoneId = MilestoneId { unMilestoneId :: Integer }
    deriving (Eq, Ord, Read, Show, Data, Typeable, PathInfo, Enum)

instance SafeCopy MilestoneId where
    getCopy = contain $ fmap MilestoneId S.get
    putCopy = contain . S.put . unMilestoneId
    errorTypeName _ = "MilestoneId"

data Milestone = Milestone
    { milestoneId      :: MilestoneId
    , milestoneTitle   :: Text
    , milestoneTarget  :: Maybe UTCTime
    , milestoneReached :: Maybe UTCTime

    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Milestone)

newtype TargetDate = TargetDate UTCTime
    deriving (Eq, Ord, Show, Data, Typeable)

instance Indexable Milestone where
    empty = ixSet [ ixFun ((:[]) . milestoneId)
                  , ixFun (maybe [] (\d -> [TargetDate d]) . milestoneTarget)
                  ]

data BugStatus
    = New
    | Accepted
    | Closed
    | Invalid
    | WontFix
      deriving (Eq, Ord, Read, Show, Data, Typeable, Bounded, Enum)

$(deriveSafeCopy 0 'base ''BugStatus)

data Bug_0
    = Bug_0 { bugId_0        :: BugId
            , bugSubmittor_0 :: UserId
            , bugSubmitted_0 :: UTCTime
            , bugStatus_0    :: BugStatus
            , bugAssigned_0  :: Maybe UserId
            , bugTitle_0     :: Text
            , bugBody_0      :: Markup
            , bugTags_0      :: Set BugTag
            , bugMilestone_0 :: Maybe MilestoneId
            }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Bug_0)

data BugMeta = BugMeta
    { bugId        :: BugId
    , bugSubmitter :: UserId
    , bugSubmitted :: UTCTime
    , bugStatus    :: BugStatus
    , bugAssigned  :: Maybe UserId
    , bugTitle     :: Text
    , bugTags      :: Set BugTag
    , bugMilestone :: Maybe MilestoneId
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''BugMeta)

data Bug = Bug
    { bugMeta        :: BugMeta
    , bugBody        :: Markup
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'extension ''Bug)

instance Migrate Bug where
    type MigrateFrom Bug = Bug_0
    migrate (Bug_0 id sub subd stat assi titl bdy tags mile) =
        Bug (BugMeta id sub subd stat assi titl tags mile) bdy

instance Indexable Bug where
    empty = ixSet [ ixFun ((:[]) . bugId . bugMeta)
                  , ixFun (maybeToList . bugMilestone . bugMeta)
                  , ixFun ((:[]) . bugStatus . bugMeta)
                  ]

instance Indexable BugMeta where
    empty = ixSet [ ixFun ((:[]) . bugId)
                  , ixFun (maybeToList . bugMilestone)
                  , ixFun ((:[]) . bugStatus)
                  ]
