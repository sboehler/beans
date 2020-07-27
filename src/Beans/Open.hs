module Beans.Open (Open (Open)) where

import Beans.Account (Account)
import Beans.Date (Date)
import Data.Text.Prettyprint.Doc (Pretty (pretty), (<+>))

data Open
  = Open
      Date
      Account
  deriving (Eq, Show, Ord)

instance Pretty Open where
  pretty (Open d a) =
    pretty d <+> "open" <+> pretty a
