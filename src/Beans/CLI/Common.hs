module Beans.CLI.Common
  ( toReadM
  ) where

import qualified Beans.Parser        as P
import qualified Data.Text           as T
import           Options.Applicative
import           Text.Megaparsec     (parse, parseErrorPretty)

toReadM :: P.Parser a -> ReadM a
toReadM p = eitherReader $ parse' p "" . T.pack
 where
   parse' parser input s = case parse parser input s of
    Left e  -> Left $ parseErrorPretty e
    Right d -> Right d
