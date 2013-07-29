--------------------------------------------------------------------
-- |
-- Module    : Text.Feed.Import
-- Copyright : (c) Galois, Inc. 2008
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Description: Convert from XML to Feeds.
--
--------------------------------------------------------------------

module Text.Feed.Import
        ( parseFeedFromFile -- :: FilePath -> IO Feed
        , parseFeedString   -- :: String -> IO Feed
	
          -- if you know your format, use these directly:
	, readRSS2          -- :: XML.Element -> Maybe Feed
	, readRSS1          -- :: XML.Element -> Maybe Feed
	, readAtom          -- :: XML.Element -> Maybe Feed
        ) where

import Text.Atom.Feed.Import as Atom
import Text.RSS.Import       as RSS
import Text.RSS1.Import      as RSS1

import Text.Feed.Types
import Text.XML.Light as XML

import Control.Monad

import System.IO.UTF8 as UTF8 ( readFile ) 

-- | 'parseFeedFromFile fp' reads in the contents of the file at @fp@;
-- the assumed encoding is UTF-8.
parseFeedFromFile :: FilePath -> IO Feed
parseFeedFromFile fp = do
  ls <- UTF8.readFile fp
  case parseFeedString ls of
    Nothing -> fail ("parseFeedFromFile: not a well-formed XML content in: " ++ fp)
    Just f  -> return f

-- | 'parseFeedString str' tries to parse the string @str@ as 
-- one of the feed formats. First as Atom, then RSS2 before
-- giving RSS1 a try. @Nothing@ is, rather unhelpfully, returned
-- as an indication of error.
parseFeedString :: String -> Maybe Feed
parseFeedString str =
  case parseXMLDoc str of
    Nothing -> Nothing
    Just e  ->
      readAtom e `mplus`
      readRSS2 e `mplus`
      readRSS1 e `mplus`
      Just (XMLFeed e)

-- | 'readRSS2 elt' tries to derive an RSS2.x, RSS-0.9x feed document
-- from the XML element @e@.
readRSS2 :: XML.Element -> Maybe Feed
readRSS2 e = fmap RSSFeed  $ RSS.elementToRSS e

-- | 'readRSS1 elt' tries to derive an RSS1.0 feed document
-- from the XML element @e@.
readRSS1 :: XML.Element -> Maybe Feed
readRSS1 e = fmap RSS1Feed $ RSS1.elementToFeed e

-- | 'readAtom elt' tries to derive an Atom feed document
-- from the XML element @e@.
readAtom :: XML.Element -> Maybe Feed
readAtom e = fmap AtomFeed $ Atom.elementFeed e

