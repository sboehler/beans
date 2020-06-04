module Beans.Position
  ( Position (Position),
    deleteLot,
  )
where

import Beans.Account (Account)
import Beans.Commodity (Commodity)
import Beans.Lot (Lot)
import Data.Text.Prettyprint.Doc ((<+>), Pretty (pretty))

data Position
  = Position Account Commodity (Maybe Lot)
  deriving (Eq, Ord, Show)

instance Pretty Position where
  pretty (Position a c l) = pretty a <+> pretty c <+> pretty l

deleteLot :: Position -> Position
deleteLot (Position a c _) = Position a c Nothing
