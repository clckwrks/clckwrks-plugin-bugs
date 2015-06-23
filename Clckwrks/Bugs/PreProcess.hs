{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Clckwrks.Bugs.PreProcess where

import Control.Monad.Trans
import Control.Applicative
import Clckwrks                         (ClckT, ClckState)
import Clckwrks.Bugs.URL                (BugsURL(..))
import Clckwrks.Bugs.Page.Timeline      (timelineWidget)
import Clckwrks.Bugs.Types              (BugId(..))
import Clckwrks.Monad                   (transform, segments)
import Data.Attoparsec.Text.Lazy        (Parser, Result(..), char, choice, decimal, parse, skipMany, space, asciiCI, skipMany)
import Data.Monoid                      (mconcat, mempty)
import           Data.Text              (Text, pack)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import HSP.HTML4                        (renderAsHTML)
import HSP.XML
import HSP.XMLGenerator
import Web.Routes                       (showURL)

data BugsCmd
    = ShowBug BugId
    | ShowTimeline
    | BugListLink

parseAttr :: Text -> Parser ()
parseAttr name =
    do skipMany space
       asciiCI name
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
    choice [ parseAttr (pack "id")     *> (ShowBug . BugId <$> decimal)
           , asciiCI (pack "timeline") *> pure ShowTimeline
           , asciiCI (pack "list-link")     *> pure BugListLink
           ]

bugsCmd :: (Functor m, Monad m) =>
           (BugsURL -> [(Text, Maybe Text)] -> Text)
        -> TL.Text
        -> ClckT url m TL.Text
bugsCmd bugsShowURL txt =
    case parse (segments "bugs" parseCmd) txt of
      (Fail _ _ e) -> return (TL.pack e)
      (Done _ segments) ->
          do b <- transform (applyCmd bugsShowURL) segments
             return $ B.toLazyText b

applyCmd bugsShowURL (ShowBug bid) =
    do html <- unXMLGenT $ <a href=(bugsShowURL (ViewBug bid) [])>#<% show $ unBugId bid  %></a>
       return $ mconcat $ map B.fromLazyText $ TL.lines $ renderAsHTML html
applyCmd bugsShowURL BugListLink =
    do html <- unXMLGenT $ <a href=(bugsShowURL BugList [])>Bug List</a>
       return $ mconcat $ map B.fromLazyText $ TL.lines $ renderAsHTML html
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
