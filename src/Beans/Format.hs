module Beans.Format
  (Section(..), formatTable, reportToTable, createReport)
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
import           Data.Text.Lazy           (Text, intercalate, pack, unpack)

data Report = Report [Position] [Section] deriving (Show)

data Section = Section
  { _labels    :: [Text]
  , _positions :: [Position]
  , _sections  :: [Section]
  } deriving (Show)

data Position = Position CommodityName (Maybe Lot) Scientific
  deriving (Show)

formatTable :: [[String]] -> String
formatTable t =
  showTable
    [ ColDesc left "Account" left (!! 0)
    , ColDesc left "Amount" right (!! 3)
    , ColDesc left "Commodity" left (!! 1)
    , ColDesc left "Lot" left (!! 2)
    ]
    t
    []

createReport :: Accounts -> Report
createReport = toReport . groupSections "" . toSections
  where
    toSections = M.elems . M.mapWithKey toSection
    toSection Key {..} amount =
      Section (maybe [] toLabel keyAccount) [Position keyCommodity keyLot amount] []
    toLabel (AccountName t ns) = pack (show t) : ns
    toReport (Section _ pos sec) = Report pos sec

reportToTable :: Report -> [[String]]
reportToTable (Report p s) = positions ++ sections
  where
    positions = formatPositions [] p
    sections = formatSection `concatMap` s

groupSections :: Text -> [Section] -> Section
groupSections title sections =
  let (rootSections, childSections) = L.partition (null . _labels) sections
      positions = concatMap _positions rootSections
      subs = concatMap _sections rootSections
      subsections = M.elems . M.mapWithKey groupSections . groupWith splitSection $ childSections
   in Section [title] positions (subsections ++ subs)

splitSection :: Section -> (Text, Section)
splitSection (Section (n:ns) positions subsections) = (n, Section ns positions subsections)
splitSection (Section [] positions subsections) =
  (mempty, Section [] positions subsections)

groupWith :: (Ord k) => (a -> (k, v)) -> [a] -> M.Map k [v]
groupWith f l = M.fromListWith (++) (second pure . f <$> l)

formatSection :: Section -> [[String]]
formatSection (Section title [] [subsection]) =
  let subs = formatSection subsection
   in case subs of
        (s:ss) -> prependFirst (unpack (intercalate ":" title) ++ ":") s : ss
        []     -> []
formatSection (Section title positions subsections) = pos ++ subs
  where
    pos = formatPositions title positions
    subs = indentFirst 2 <$> (formatSection `concatMap` subsections)

formatPositions :: [Text] -> [Position] -> [[String]]
formatPositions title (Position c l s:ps) = [unpack (intercalate ":" title), show c, maybe "" show l, formatStandard s] : formatPositions [] ps
formatPositions [] [] = []
formatPositions title [] = [[unpack (intercalate ":" title), "", "", ""]]

indentFirst :: Int -> [String] -> [String]
indentFirst n = prependFirst (replicate n ' ')

prependFirst :: String -> [String] -> [String]
prependFirst p (s:ss) = (p++s):ss
prependFirst _ []     = []
