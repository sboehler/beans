module Lib
  ( doParse
  ) where

import Data.Either (lefts, rights)
import qualified Data.Map.Lazy as M
import Data.Text.Lazy.IO (readFile)
import Data.Text.Prettyprint.Doc
import Data.Time.Calendar (Day)
import Parser (parse')
import Parser.AST
       (ConfigDirective(..), DatedDirective(..), Directive(..),
        Directive(Config, Dated))
import Parser.Interpreter (completeTransaction)
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
      let dated = [d | d@(Dated _ _) <- directives]
      let complete = completeTransaction <$> dated
      print $ vsep (((<+> hardline) . pretty) <$> lefts complete)
      print $ length directives

newtype DatedMap =
  DatedMap (M.Map Day [DatedDirective])
  deriving (Show)

instance Pretty DatedMap where
  pretty (DatedMap m) = M.foldlWithKey f emptyDoc m
    where
      f doc d dds = doc <> cat (map (ppDir d) dds) <> hardline
      ppDir day dir = pretty (show day) <+> pretty dir <> hardline
