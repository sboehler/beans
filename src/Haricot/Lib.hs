module Haricot.Lib
  ( doParse
  ) where

import           Control.Monad.Catch       (MonadThrow)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Trans       (liftIO)
import           Data.Text.Lazy.IO         (readFile)
import           Data.Text.Prettyprint.Doc
import           Haricot.AST               (Directive (..), Include (..))
import           Haricot.Parser            (parseFile)
import           Haricot.Pretty            ()
import           Prelude                   hiding (readFile)
import           System.Environment        (getArgs)
import           System.FilePath.Posix     (takeDirectory, (</>))
import qualified Text.Megaparsec           as P

collectRelativePaths :: [Directive a] -> [FilePath]
collectRelativePaths directives =
  [relPath | (Inc (Include relPath) _) <- directives]

recursiveParse ::
     (MonadIO m, MonadThrow m) => FilePath -> m [Directive P.SourcePos]
recursiveParse filePath = do
  fileContent <- liftIO $ readFile filePath
  directives <- parseFile filePath fileContent
  let dirPath = takeDirectory filePath
      absPaths = (dirPath </>) <$> collectRelativePaths directives
  (directives ++) . concat <$> traverse recursiveParse absPaths

prettyPrint :: [Directive P.SourcePos] -> IO ()
prettyPrint = print . vsep . map ((<> hardline) . pretty)

doParse :: (MonadIO m, MonadThrow m) => m ()
doParse = do
  (file:_) <- liftIO getArgs
  liftIO $ prettyPrint =<< recursiveParse file
