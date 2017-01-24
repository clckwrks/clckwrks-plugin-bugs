{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeFamilies, TypeSynonymInstances, UndecidableInstances, OverloadedStrings #-}
module Clckwrks.Bugs.Monad where

import Clckwrks                 (Clck, ClckT(..), ClckFormT, ClckState(..), ClckURL(..), mapClckT)
import Clckwrks.Acid
import Clckwrks.IOThread        (IOThread(..), startIOThread, killIOThread)
import Clckwrks.Bugs.Acid
import Clckwrks.Bugs.Types
import Clckwrks.Bugs.URL
import Control.Applicative ((<$>))
import Control.Exception   (bracket)
import Control.Monad.Reader (ReaderT(..), MonadReader(..))
import Data.Acid           (AcidState)
import Data.Acid.Local     (createCheckpointAndClose, openLocalStateFrom)
import qualified Data.Map  as Map
import Data.Maybe          (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Happstack.Server
import Happstack.Server.Internal.Monads (FilterFun)
import HSP.XMLGenerator     (Attr((:=)), EmbedAsAttr(..), EmbedAsChild(..), IsName(toName), XMLGen, XMLGenT)
import HSP.XML              (Attribute(MkAttr), XML, pAttrVal)
import System.Directory     (createDirectoryIfMissing)
import System.FilePath      ((</>))
import Text.Reform          (CommonFormError, FormError(..))
import Web.Routes           (URL, MonadRoute, showURL)

data BugsConfig = BugsConfig
    { bugsDirectory    :: FilePath -- ^ directory in which to store uploaded attachments
    , bugsState        :: AcidState BugsState
    , bugsClckURL      :: ClckURL -> [(T.Text, Maybe T.Text)] -> T.Text
    }

type BugsT m = ClckT BugsURL (ReaderT BugsConfig m)
type BugsM   = ClckT BugsURL (ReaderT BugsConfig (ServerPartT IO))

data BugsFormError
    = BugsCFE (CommonFormError [Input])
      deriving Show

instance FormError BugsFormError where
    type ErrorInputType BugsFormError = [Input]
    commonFormError = BugsCFE

instance (Functor m, Monad m) => EmbedAsChild (BugsT m) BugsFormError where
    asChild e = asChild (show e)

type BugsForm = ClckFormT BugsFormError BugsM

instance (IsName n TL.Text) => EmbedAsAttr BugsM (Attr n BugsURL) where
        asAttr (n := u) =
            do url <- showURL u
               asAttr $ MkAttr (toName n, pAttrVal (TL.fromStrict url))

instance (IsName n TL.Text) => EmbedAsAttr BugsM (Attr n ClckURL) where
        asAttr (n := url) =
            do showFn <- bugsClckURL <$> ask
               asAttr $ MkAttr (toName n, pAttrVal (TL.fromStrict $ showFn url []))

instance (XMLGen m, Functor m, Monad m, EmbedAsChild m String) => EmbedAsChild m BugId where
    asChild (BugId i) = asChild $ '#' : show i

runBugsT :: BugsConfig -> BugsT m a -> ClckT BugsURL m a
runBugsT mc m = mapClckT f m
    where
      f r = runReaderT r mc

instance (Monad m) => MonadReader BugsConfig (BugsT m) where
    ask = ClckT $ ask
    local f (ClckT m) = ClckT $ local f m

instance (Functor m, Monad m) => GetAcidState (BugsT m) BugsState where
    getAcidState =
        bugsState <$> ask
{-
withBugsConfig :: Maybe FilePath
               -> FilePath
               -> (BugsConfig -> IO a) -> IO a
withBugsConfig mBasePath bugsDir f =
    do let basePath = fromMaybe "_state" mBasePath
       bracket (openLocalStateFrom (basePath </> "bugs") initialBugsState) (createCheckpointAndClose) $ \bugsState ->
           f (BugsConfig { bugsDirectory    = bugsDir
                         , bugsState        = bugsState
                         , bugsClckURL      = undefined
--                         , bugsPageTemplate = undefined
                         })
-}
