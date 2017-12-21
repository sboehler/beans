module Lib
  ( doParse
  ) where

import qualified Data.Map.Lazy as M
import Data.Text.Lazy.IO (readFile)
import Data.Time.Calendar (Day, fromGregorian)
import Parser
       (ConfigDirective(..), DatedDirective(..), Directive(Config, Dated),
        parse')
import Prelude hiding (readFile)
import System.Environment (getArgs)
import System.FilePath.Posix ((</>), takeDirectory)
import Text.Parsec (ParseError)

import Control.Monad.Except (ExceptT(..), runExceptT)

getIncludeFiles :: FilePath -> [Directive] -> [FilePath]
getIncludeFiles f (d:ds) =
  case d of
    Config (Include p) -> (takeDirectory f </> p) : getIncludeFiles f ds
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
      let d = getDatedTransactions directives
      print $ M.lookupLT (fromGregorian 2017 12 1) d
      print $ length directives

getDatedTransactions :: [Directive] -> M.Map Day [DatedDirective]
getDatedTransactions arg = M.fromListWith (++) $ foldl fil [] arg
  where
    fil l (Dated d dd) = (d, [dd]) : l
    fil l _ = l
