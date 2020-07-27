module Beans.Price (Price (Price)) where

import Beans.Commodity (Commodity)
import Beans.Date (Date)
import Data.Text.Prettyprint.Doc (Pretty (pretty), (<+>))

data Price
  = Price
      Date
      Commodity
      Double
      Commodity
  deriving (Eq, Show, Ord)

instance Pretty Price where
  pretty (Price d c p tc) =
    pretty d
      <+> "price"
      <+> pretty c
      <+> pretty p
      <+> pretty tc
