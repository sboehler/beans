module Haricot.Lib
  ( parse
  ) where

import           Control.Monad.Catch       (MonadThrow)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Trans       (liftIO)
import           Haricot.Parser            (parseFiles)
import           Haricot.Pretty            (prettyPrint)
import           System.Environment        (getArgs)


parse :: (MonadIO m, MonadThrow m) => m ()
parse = do
  (file:_) <- liftIO getArgs
  ast <- parseFiles file
  liftIO $ prettyPrint ast
