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
  d = (format . createReport . toItems) accounts
  desc = showTable
    [ ColDesc left "Account" left (!!0)
    , ColDesc left "Amount" right (!!3)
    , ColDesc left "Commodity" left (!!1)
    , ColDesc left "Lot" left (!!2)
    ]
 in
  desc d []

toItems :: Accounts -> [Item]
toItems = fmap f . M.toList
  where
    f (Key {..}, amount) =
      Item (maybe [] toLabel keyAccount) [Position keyCommodity keyLot amount]

toLabel :: AccountName -> [Text]
toLabel (AccountName t ns) = pack (show t) : ns

createReport :: [Item] -> Section
createReport = groupItems "Report"

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
indentFirst n (l:ll) = indent n l: ll
indentFirst _ []     = []

indent :: Int -> String -> String
indent 0 s = s
indent n s = ' ' : indent (n - 1) s
