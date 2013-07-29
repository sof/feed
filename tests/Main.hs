module Main where

import Text.XML.Light
import Text.Feed.Import
import Text.Feed.Export

import System.Environment

main :: IO ()
main = do
  (x:_) <- getArgs
  feed  <- parseFeedFromFile x
  putStrLn (ppTopElement $ xmlFeed feed)
