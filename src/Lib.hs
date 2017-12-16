module Lib
  ( doParse
  ) where

import Data.Text.Lazy.IO (readFile)
import Parser (Directive, parse')
import Prelude hiding (readFile)
import System.Environment (getArgs)
import Text.Parsec (ParseError)

parseFile :: FilePath -> IO (Either ParseError [Directive])
parseFile f = parse' f <$> readFile f

doParse :: IO ()
doParse = do
  (file:_) <- getArgs
  result <- parseFile file
  case result of
    Left err -> print err
    Right directives -> print directives
