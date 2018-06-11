module Beans.Format
  (Section(..), formatReport )
where

import           Beans.Accounts           (Accounts, Key (..))
import           Beans.AST                (AccountName (..), CommodityName (..),
                                           Lot (..))
import           Beans.Table              (ColDesc (..), formatStandard, left,
                                           right, showTable)
import           Data.Bifunctor           (second)
import qualified Data.List                as L
import qualified Data.Map.Strict.Extended as M
import           Data.Scientific.Extended (Scientific)
import           Data.Text.Lazy           (Text, pack, unpack)


data Section =
  Section Text
          [Position]
          [Section]
  deriving (Show)

data Item = Item
  { _labels    :: [Text]
  , _positions :: [Position]
  } deriving (Show)

data Position = Position CommodityName (Maybe Lot) Scientific
  deriving (Show)

formatReport :: Accounts -> String
formatReport accounts = let
  d = (format . createReport) accounts
  desc = showTable
    [ ColDesc left "Account" left (!!0)
    , ColDesc left "Amount" right (!!3)
    , ColDesc left "Commodity" left (!!1)
    , ColDesc left "Lot" left (!!2)
    ]
 in
  desc d []

createReport :: Accounts -> Section
createReport = groupItems "Report" . toItems
  where
    toItems = fmap toItem . M.toList
    toItem (Key {..}, amount) =
      Item (maybe [] toLabel keyAccount) [Position keyCommodity keyLot amount]
    toLabel (AccountName t ns) = pack (show t) : ns

groupItems :: Text -> [Item] -> Section
groupItems title items =
  let (rootItems, childItems) = L.partition (null . _labels) items
      positions = concatMap _positions rootItems
      subSections = groupWith splitItem childItems
   in Section title positions (M.elems $ M.mapWithKey groupItems subSections)

splitItem :: Item -> (Text, Item)
splitItem (Item (n:ns) positions) = (n, Item ns positions)
splitItem (Item [] positions)     = (mempty, Item [] positions)

groupWith :: (Ord k) => (a -> (k, v)) -> [a] -> M.Map k [v]
groupWith f l = M.fromListWith (++) (second pure . f <$> l)

format :: Section -> [[String]]
format (Section title positions subsections) =
  let col123 = (\(Position c l s) -> [show c, show l, formatStandard s]) <$> positions
      cols =
        case col123 of
          [] -> [unpack title : replicate 3 ""]
          l  -> zipWith (:) (unpack title : repeat "") l
      subs = indentFirst 2 <$> concat (format <$> subsections)
   in cols ++ subs

indentFirst :: Int -> [String] -> [String]
indentFirst 0 s      = s
indentFirst n (l:ll) = indentFirst (n - 1) ((' ':l): ll)
indentFirst _ []     = []
