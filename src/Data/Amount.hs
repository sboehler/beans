module Data.Amount
  ( Amount(..)
  , (<@@>)
  ) where

import Data.Commodity (CommodityName)
import Data.Text.Prettyprint.Doc (Pretty, (<+>), pretty)

data Amount a = Amount
  { _amount :: a
  , _commodity :: CommodityName
  } deriving (Eq, Show, Functor)

instance Show a => Pretty (Amount a) where
  pretty Amount {..} = pretty (show _amount) <+> pretty _commodity

(<@@>) :: Num a => Amount a -> Amount a -> Amount a
(Amount a _) <@@> amount = (signum a *) <$> amount
