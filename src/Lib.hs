module Lib
  ( doParse
  ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (liftIO)
import qualified Data.Map.Lazy as M
import Data.Text.Lazy.IO (readFile)
import Data.Text.Prettyprint.Doc
import Data.Time.Calendar (Day)
import Parser (parse)
import Parser.AST (Directive(..), Include(..))
import Prelude hiding (readFile)
import System.Environment (getArgs)
import System.FilePath.Posix ((</>), takeDirectory)
import qualified Text.Parsec as P

getIncludeFiles :: FilePath -> [Directive a] -> [FilePath]
getIncludeFiles path ds = [takeDirectory path </> f | (Inc (Include f) _) <- ds]

recursiveParse ::
     (MonadIO m, MonadThrow m) => FilePath -> m [Directive P.SourcePos]
recursiveParse f = do
  directives <- liftIO (readFile f) >>= parse f
  others <- traverse recursiveParse . getIncludeFiles f $ directives
  return $ directives ++ concat others

prettyPrint :: [Directive P.SourcePos] -> IO ()
prettyPrint = print . vsep . map ((<> hardline) . pretty)

doParse :: (MonadIO m, MonadThrow m) => m ()
doParse = do
  (file:_) <- liftIO getArgs
  liftIO $ prettyPrint =<< recursiveParse file

newtype DatedMap a =
  DatedMap (M.Map Day [Directive a])
  deriving (Show)

instance Pretty (DatedMap a) where
  pretty (DatedMap m) = M.foldlWithKey f emptyDoc m
    where
      f doc d dds = doc <> cat (map (ppDir d) dds) <> hardline
      ppDir day dir = pretty (show day) <+> pretty dir <> hardline
