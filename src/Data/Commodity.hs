module Data.Commodity where

import Control.Lens (makeLenses)
import Data.Text.Lazy (Text)

newtype CommodityName = CommodityName
  { _unCommodityName :: Text
  } deriving (Show, Eq, Ord)

makeLenses ''CommodityName
