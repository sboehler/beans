module Beans.Import.Common
  ( ImporterException(..)
  , Context(..)
  , Config(..)
  )
where

import           Beans.Model                              ( Account )
import           Control.Exception                        ( Exception )
import           Beans.Import.DSL                         ( Evaluator
                                                          , Context(..)
                                                          )

-- The exception exported by this module
newtype ImporterException =
  ImporterException String
  deriving (Eq)

instance Show ImporterException where
  show (ImporterException s) = s

instance Exception ImporterException

data Config = Config {
  cEvaluator :: Evaluator,
  cFile :: FilePath,
  cAccount :: Account
  }
