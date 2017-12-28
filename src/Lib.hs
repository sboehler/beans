module Lib
  ( fn
  ) where

import Control.Monad.Trans (liftIO)
import qualified Data.Map.Lazy as M
import Data.Text.Lazy.IO (readFile)
import Data.Text.Prettyprint.Doc
import Data.Time.Calendar (Day)
import Parser (parse)
import Parser.AST (Directive(..), Include(..))
import Parser.Interpreter (completeTransaction)
import Prelude hiding (readFile)
import System.Environment (getArgs)
import System.FilePath.Posix ((</>), takeDirectory)
import qualified Text.Parsec as P

import Control.Monad.Except
       (ExceptT(..), catchError, runExceptT, throwError, void)

getIncludeFiles :: FilePath -> [Directive a] -> [FilePath]
getIncludeFiles currentPath (d:ds) =
  case d of
    Inc (Include filePath) _ ->
      (takeDirectory currentPath </> filePath) : getIncludeFiles currentPath ds
    _ -> getIncludeFiles currentPath ds
getIncludeFiles _ [] = []

recursiveParse :: FilePath -> ExceptT P.ParseError IO [Directive P.SourcePos]
recursiveParse f = do
  directives <- ExceptT $ parse f <$> readFile f
  d <-
    concat . (directives :) <$>
    traverse recursiveParse (getIncludeFiles f directives)
  case traverse completeTransaction d of
    Left e -> throwError e
    Right l -> return l

prettyPrint :: [Directive P.SourcePos] -> IO ()
prettyPrint = print . vsep . map ((<> hardline) . pretty)

doParse :: ExceptT P.ParseError IO ()
doParse = do
  (file:_) <- liftIO getArgs
  directives <- recursiveParse file
  liftIO $ prettyPrint directives

fn :: IO ()
fn = void $ runExceptT $ doParse `catchError` (liftIO . print)

newtype DatedMap a =
  DatedMap (M.Map Day [Directive a])
  deriving (Show)

instance Pretty (DatedMap a) where
  pretty (DatedMap m) = M.foldlWithKey f emptyDoc m
    where
      f doc d dds = doc <> cat (map (ppDir d) dds) <> hardline
      ppDir day dir = pretty (show day) <+> pretty dir <> hardline
