module Beans.Lot (Lot (Lot)) where

import Beans.Amount (Amount)
import Beans.Commodity (Commodity)
import Beans.Date (Date)
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty (pretty), hsep)

data Lot
  = Lot
      { price :: Amount,
        targetCommodity :: Commodity,
        date :: Date,
        label :: Maybe Text
      }
  deriving (Eq, Ord)

instance Show Lot where
  show Lot {price, targetCommodity, date, label} =
    let p = show price ++ " " ++ show targetCommodity
        elems = catMaybes [Just p, Just $ show date, show <$> label]
     in "{ " ++ L.intercalate ", " elems ++ " }"

instance Pretty Lot where
  pretty Lot {label, price, targetCommodity, date} =
    hsep
      ( ["{", pretty price, pretty targetCommodity, ",", pretty date]
          ++ case label of
            Nothing -> ["}"]
            Just l -> [",", pretty l, "}"]
      )
