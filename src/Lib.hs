module Lib
  ( fn
  ) where

import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)
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

getIncludeFiles :: FilePath -> [Directive a] -> [FilePath]
getIncludeFiles currentPath (d:ds) =
  case d of
    Inc (Include filePath) _ ->
      (takeDirectory currentPath </> filePath) : getIncludeFiles currentPath ds
    _ -> getIncludeFiles currentPath ds
getIncludeFiles _ [] = []

recursiveParse ::
     (MonadIO m, MonadThrow m) => FilePath -> m [Directive P.SourcePos]
recursiveParse f = do
  t <- liftIO $ readFile f
  directives <- parse f t
  others <- traverse recursiveParse (getIncludeFiles f directives)
  let d = directives ++ concat others
  case traverse completeTransaction d of
    Left e -> throwM e
    Right l -> return l

prettyPrint :: [Directive P.SourcePos] -> IO ()
prettyPrint = print . vsep . map ((<> hardline) . pretty)

doParse :: (MonadIO m, MonadThrow m) => m ()
doParse = do
  (file:_) <- liftIO getArgs
  directives <- recursiveParse file
  liftIO $ prettyPrint directives

fn :: IO ()
fn = doParse

newtype DatedMap a =
  DatedMap (M.Map Day [Directive a])
  deriving (Show)

instance Pretty (DatedMap a) where
  pretty (DatedMap m) = M.foldlWithKey f emptyDoc m
    where
      f doc d dds = doc <> cat (map (ppDir d) dds) <> hardline
      ppDir day dir = pretty (show day) <+> pretty dir <> hardline
