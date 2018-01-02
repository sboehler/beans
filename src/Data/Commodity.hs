module Data.Commodity where

import Data.Text.Lazy (Text)
import Data.Text.Prettyprint.Doc

newtype CommodityName = CommodityName
  { _unCommodityName :: Text
  } deriving (Show, Eq, Ord)

instance Pretty CommodityName where
  pretty = pretty . _unCommodityName
