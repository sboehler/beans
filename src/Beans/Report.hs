module Beans.Report
  ( fromBalances,
    toTable,
    Report,
    Options (..),
    Collapse,
    Format (..),
  )
where

import qualified Beans.ATree as ATree
import Beans.ATree (ATree (..))
import qualified Beans.Account as Account
import Beans.Amount (Amount)
import Beans.Balance (Balance (Balance))
import Beans.Commodity (Commodity)
import Beans.Date (Date)
import Beans.Filter (AccountFilter)
import Beans.Position (Position (Position))
import qualified Beans.Positions as Positions
import Beans.Positions (Positions)
import Beans.Table (Cell (..), Row (Row), Table (Table))
import Beans.ValAmount (ValAmount (ValAmount))
import Control.Monad.Reader (MonadReader, ask, asks)
import qualified Data.List as List
import qualified Data.Map.Strict.Extended as Map
import Data.Map.Strict.Extended ((!))
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Prettyprint.Doc as Pretty
import Data.Text.Prettyprint.Doc (Pretty)
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import Data.Traversable (for)

--------------------------------------------------------------------------------

data Options
  = Options
      { valuation :: [Commodity],
        showCommodities :: Bool,
        balanceFormat :: Format,
        collapse :: Collapse
      }
  deriving (Show)

type Collapse = [(AccountFilter, Int)]

data Format = Flat | Hierarchical deriving (Show)

data Report
  = Report
      [Date]
      [ATree Text (Positions [Amount])]

fromBalances :: MonadReader Options m => [Balance ValAmount] -> m Report
fromBalances b = do
  val <- asks valuation
  let positions = Positions.concatenate $ fmap (valueOrAmount val) . getPosition <$> b
  segments <- segment positions
  let tree = ATree.fromList segments
  pure $ Report dates tree
  where
    dates = getDate <$> b
    getPosition (Balance _ p _ _ _) = p
    getDate (Balance d _ _ _ _) = d

valueOrAmount :: [Commodity] -> ValAmount -> Amount
valueOrAmount [] (ValAmount a _) = a
valueOrAmount (c : _) (ValAmount _ v) = v ! c

segment :: MonadReader Options m => Positions v -> m [([Text], Positions v)]
segment m = for (Map.toList m) $ \(pos, v) -> do
  tag <- createTag pos
  pure (tag, Map.singleton pos v)

createTag :: MonadReader Options m => Position -> m [Text]
createTag pos@(Position a _ _) = do
  f <- asks balanceFormat
  segments <- shorten pos (Account.split a)
  pure $ case f of
    Flat -> [Text.intercalate ":" segments]
    Hierarchical -> segments

shorten :: MonadReader Options m => Position -> [a] -> m [a]
shorten (Position a _ _) tag = asks (foldr g tag . collapse)
  where
    g (regex, depth) t
      | Account.match regex a = take depth t
      | otherwise = t

--------------------------------------------------------------------------------

toTable :: MonadReader Options m => Report -> m Table
toTable (Report dates segments) = do
  (childRows, childPos) <- unzip <$> for segments (treeToRows nbrColumns 0)
  footers <- positionToRows nbrColumns 0 "Total" (total nbrColumns childPos)
  let colgroups = 0 : replicate nbrColumns 1
      rows = sep : List.intercalate [sep] ([[header]] <> childRows <> [footers]) ++ pure sep
  pure $ Table colgroups rows
  where
    header = Row (AlignLeft "Account" : (AlignLeft . render <$> dates))
    sep = Row $ replicate (nbrColumns + 1) Separator
    nbrColumns = length dates

treeToRows ::
  (Monoid a, Eq a, Num a, Pretty a, MonadReader Options m) =>
  Int ->
  Int ->
  ATree Text (Positions [a]) ->
  m ([Row], Positions [a])
treeToRows nbrColumns indent (ATree key positions ch) = do
  (childrows, childpos) <- unzip <$> for ch (treeToRows nbrColumns (indent + 2))
  posrows <- positionToRows nbrColumns indent key positions
  let rows = mconcat (posrows : childrows)
      pos = total nbrColumns (positions : childpos)
  pure (rows, pos)

positionToRows ::
  (Monoid a, Eq a, Num a, Pretty a) =>
  MonadReader Options m =>
  Int ->
  Int ->
  Text ->
  Positions [a] ->
  m [Row]
positionToRows nbrColumns indent name positions = do
  Options {showCommodities, valuation} <- ask
  pure $ if showCommodities || length valuation == 0 then multiline else oneline
  where
    multiline = title : Map.elems posrows
      where
        title = Row (IndentBy indent name : replicate nbrColumns Empty)
        amountByCommodity = Map.mapKeysWith (addAmounts nbrColumns) getCommodity positions
        posrows = Map.mapWithKey (lineToRow indent) amountByCommodity
        getCommodity (Position _ c _) = c
    oneline = [Row (title : pos)]
      where
        title = IndentBy indent name
        pos =
          if null positions
            then replicate nbrColumns Empty
            else amountsToCells . totalAmount $ positions

addAmounts :: Monoid a => Int -> [a] -> [a] -> [a]
addAmounts n a1 a2 = zipWith (<>) (extend n a1) (extend n a2)

extend :: Monoid a => Int -> [a] -> [a]
extend n = take n . (<> repeat mempty)

totalAmount :: Monoid a => Positions [a] -> [a]
totalAmount m = mconcat <$> List.transpose (Map.elems m)

lineToRow :: (Eq a, Pretty a, Num a) => Int -> Commodity -> [a] -> Row
lineToRow indent c amt = Row (commodity : amounts)
  where
    commodity = IndentBy (indent + 2) . render $ c
    amounts = amountsToCells amt

amountsToCells :: (Eq a, Pretty a, Num a) => [a] -> [Cell]
amountsToCells amounts = toCell <$> amounts
  where
    toCell 0 = Empty
    toCell a = (AlignRight . render) a

total :: Monoid a => Int -> [Positions [a]] -> Positions [a]
total n = foldr (Map.unionWith (addAmounts n)) mempty

render :: Pretty a => a -> Text
render = Pretty.renderStrict . Pretty.layoutCompact . Pretty.pretty
