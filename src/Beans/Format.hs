module Beans.Format
  (Section(..), formatReport, createReport)
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

data Report = Report [Position] [Section] deriving (Show)

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

formatReport :: Report -> String
formatReport (Report pos sec) = showTable
    [ ColDesc left "Account" left (!!0)
    , ColDesc left "Amount" right (!!3)
    , ColDesc left "Commodity" left (!!1)
    , ColDesc left "Lot" left (!!2)
    ] (formatPositions "" pos ++ concatMap format sec) []

createReport :: Accounts -> Report
createReport = toReport . groupItems "" . toItems
  where
    toItems = fmap toItem . M.toList
    toItem (Key {..}, amount) =
      Item (maybe [] toLabel keyAccount) [Position keyCommodity keyLot amount]
    toLabel (AccountName t ns) = pack (show t) : ns
    toReport (Section _ pos sec) = Report pos sec

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
  let
    pos = formatPositions title positions
    subs = indentFirst 2 <$> concat (format <$> subsections)
  in pos ++ subs

formatPositions :: Text -> [Position] -> [[String]]
formatPositions title (Position c l s:ps) = [unpack title, show c, maybe "" show l, formatStandard s] : formatPositions "" ps
formatPositions title []
  | title == "" = []
  | otherwise = [[unpack title, "", "", ""]]


indentFirst :: Int -> [String] -> [String]
indentFirst 0 s      = s
indentFirst n (l:ll) = indentFirst (n - 1) ((' ':l): ll)
indentFirst _ []     = []
