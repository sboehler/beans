module Beans.CLI.Common
  ( toReadM
  ) where

import qualified Beans.Parser        as P
import qualified Data.Text           as T
import           Options.Applicative
import           Text.Megaparsec     (parseMaybe)

toReadM :: P.Parser a -> ReadM a
toReadM p = maybeReader $ parseMaybe p . T.pack
