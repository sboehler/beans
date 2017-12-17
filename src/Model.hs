module Model where

import Data.Decimal (Decimal)
import Data.Map (Map)
import Data.Time.Calendar (Day)
import Parser (AccountName, CommodityName, Directive(..))

newtype Holdings =
  Holdings (Map AccountName (Map CommodityName Decimal))
-- calculateHolding :: [Directive] -> Day -> Holdings
