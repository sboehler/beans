module Beans.Close (Close (Close)) where

import Beans.Account (Account)
import Beans.Date (Date)
import Data.Text.Prettyprint.Doc ((<+>), Pretty (pretty))

data Close
  = Close
      Date
      Account
  deriving (Eq, Show, Ord)

instance Pretty Close where
  pretty (Close d a) = "close" <+> pretty d <+> pretty a
