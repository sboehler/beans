module Lib
  ( doParse
  ) where

import Data.Text.Lazy.IO (readFile)
import Parser (Directive(Include), parse')
import Prelude hiding (readFile)
import System.Environment (getArgs)
import System.FilePath.Posix ((</>), takeDirectory)
import Text.Parsec (ParseError)

import Control.Monad.Except (ExceptT(..), runExceptT)

getIncludeFiles :: FilePath -> [Directive] -> [FilePath]
getIncludeFiles f (d:ds) =
  case d of
    Include p -> (takeDirectory f </> p) : getIncludeFiles f ds
    _ -> getIncludeFiles f ds
getIncludeFiles _ [] = []

parseFile :: FilePath -> ExceptT ParseError IO [Directive]
parseFile f = do
  directives <- ExceptT $ parse' f <$> readFile f
  others <- mapM parseFile (getIncludeFiles f directives)
  return $ mconcat (directives : others)

doParse :: IO ()
doParse = do
  (file:_) <- getArgs
  result <- runExceptT $ parseFile file
  case result of
    Left err -> print err
    Right directives -> do
      print directives
      print $ length directives
