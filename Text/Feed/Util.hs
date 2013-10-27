--------------------------------------------------------------------
-- |
-- Module    : Text.Feed.Util
-- Copyright : (c) Galois, Inc. 2008,
--             (c) Sigbjorn Finne 2009-
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
--------------------------------------------------------------------
module Text.Feed.Util where

import Text.Feed.Types
import System.Time
import System.Locale

-- | 'toFeedDate' translates a calendar time into
-- the format expected by the feed kind.
toFeedDateString :: FeedKind -> ClockTime -> {-Date-}String
toFeedDateString fk ct =
  case fk of
    AtomKind{} -> formatCalendarTime defaultTimeLocale "%Y-%m-%dT%H:%M:%sZ" (toUTCTime ct)
    RSSKind{}  -> formatCalendarTime defaultTimeLocale "%a, %d %b %Y %H:%m:%s GMT" (toUTCTime ct)
    RDFKind{}  -> formatCalendarTime defaultTimeLocale "%Y-%m-%dT%H:%M:%sZ" (toUTCTime ct)
