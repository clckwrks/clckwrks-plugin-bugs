{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Bugs.PreProcess where

import Control.Monad.Trans
import Control.Applicative
import Clckwrks                         (ClckT, ClckState)
import Clckwrks.Bugs.URL                (BugsURL(..))
import Clckwrks.Bugs.Page.Timeline      (timelineWidget)
import Clckwrks.Bugs.Types              (BugId(..))
import Clckwrks.Monad                   (transform, segments)
import Data.Attoparsec.Text.Lazy        (Parser, Result(..), char, choice, decimal, parse, skipMany, space, stringCI, skipMany)
import Data.Monoid                      (mempty)
import           Data.Text              (Text, pack)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import HSP
import HSP.HTML                         (renderAsHTML)
import HSP.Identity                     (evalIdentity)
import Web.Routes                       (showURL)

data BugsCmd
    = ShowBug BugId
    | ShowTimeline

parseAttr :: Text -> Parser ()
parseAttr name =
    do skipMany space
       stringCI name
       skipMany space
       char '='
       skipMany space
{-
bugsCmd :: (BugsURL -> [(Text, Maybe Text)] -> Text) -> Parser Builder
bugsCmd showBugsURL =
    bugId showBugsURL

bugId :: (BugsURL -> [(Text, Maybe Text)] -> Text) -> Parser Builder
bugId showBugsURL =
    do parseAttr "id"
       bid <- BugId <$> decimal
       let html = evalIdentity $ <a href=(showBugsURL (ViewBug bid) [])>#<% show $ unBugId bid  %></a>
       return $ B.fromString $ concat $ lines $ renderAsHTML html
-}
parseCmd :: Parser BugsCmd
parseCmd =
    choice [ parseAttr (pack "id") *> (ShowBug . BugId <$> decimal)
           , stringCI (pack "timeline") *> pure ShowTimeline
           ]

bugsCmd :: (Functor m, Monad m) =>
           (BugsURL -> [(Text, Maybe Text)] -> Text)
        -> Text
        -> ClckT url m Text
bugsCmd bugsShowURL txt =
    case parse (segments "bugs" parseCmd) (TL.fromStrict txt) of
      (Fail _ _ e) -> return (T.pack e)
      (Done _ segments) ->
          do b <- transform (applyCmd bugsShowURL) segments
             return $ TL.toStrict (B.toLazyText b)

applyCmd bugsShowURL (ShowBug bid) =
    do html <- unXMLGenT $ <a href=(bugsShowURL (ViewBug bid) [])>#<% show $ unBugId bid  %></a>
       return $ B.fromString $ concat $ lines $ renderAsHTML html
{-
applyCmd bugsShowURL ShowTimeline =
    do html <- unXMLGenT $ timelineWidget
       return $ B.fromString $ concat $ lines $ renderAsHTML html
-}



-- timeline :: 
-- timeline 

{-
parseCmd :: Parser BugsCmd
parseCmd =
    choice [ parseAttr (pack "id") *> (ShowBug . BugId <$> decimal)
           , stringCI (pack "timeline") *> pure ShowTimeline
           ]

data BugsCmd
    = ShowBug BugId
    | ShowTimeline

bugsCmd :: (Functor m, Monad m) => (BugsURL -> [(Text, Maybe Text)] -> Text) -> Text -> ClckT url m Builder
bugsCmd
 showURLFn txt =
    do let mi = parseOnly parseCmd txt
       case mi of
         (Left e) ->
               return $ B.fromString e -- FIXME: format the error more nicely or something?
         (Right (ShowBug bid)) ->
             do html <- unXMLGenT $ <a href=(showURLFn (ViewBug bid) [])>#<% show $ unBugId bid  %></a>
                return $ B.fromString $ concat $ lines $ renderAsHTML html
{-
         -- types are not setup to allow us to do this yet :(
         (Right ShowTimeline) ->
             do html <- unXMLGenT $ timelineWidget
                return $ B.fromString $ concat $ lines $ renderAsHTML html
-}

-}