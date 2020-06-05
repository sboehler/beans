module Beans.Commodity
  ( Commodity (Commodity),
    match,
  )
where

import Beans.Filter (CommodityFilter (CommodityFilter))
import Data.Text (Text, unpack)
import Data.Text.Prettyprint.Doc (Pretty (pretty))
import qualified Dhall
import GHC.Generics (Generic)
import Text.Regex.PCRE ((=~))

newtype Commodity = Commodity Text
  deriving (Eq, Ord, Generic)

instance Dhall.FromDhall Commodity

instance Show Commodity where
  show (Commodity n) = unpack n

instance Pretty Commodity where
  pretty (Commodity c) = pretty c

match :: CommodityFilter -> Commodity -> Bool
match (CommodityFilter regex) = (=~ regex) . show
