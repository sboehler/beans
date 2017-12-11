module Lib
  ( doParse
  ) where

import Data.Text (pack)
import Parser (parse')
import System.Environment (getArgs)

doParse :: IO ()
doParse = do
  (file:_) <- getArgs
  s <- readFile file
  case parse' file (pack s) of
    Left x -> print x
    Right t -> print t
