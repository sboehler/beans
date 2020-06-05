module Beans.Transaction
  ( Posting (Posting),
    Tag (Tag),
    Transaction (Transaction),
    accounts,
    createAdjustment,
    createBalanced,
    match,
    valuate,
  )
where

import Beans.Account (Account)
import qualified Beans.Account as Account
import Beans.Amount (Amount)
import qualified Beans.Commodity as Commodity
import Beans.Commodity (Commodity)
import Beans.Date (Date)
import Beans.Filter (Filter (Filter))
import Beans.Lot (Lot)
import Beans.Position (Position (..))
import qualified Beans.ValAmount as ValAmount
import Beans.ValAmount (ValAmount)
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Data.Group (invert)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc ((<+>), Pretty (pretty), dquotes, hardline, hsep, pretty, vsep)
import qualified Data.Text.Prettyprint.Doc as Pretty
import Data.Traversable (for)

data Transaction
  = Transaction Date Text [Tag] [Posting]
  deriving (Show, Eq)

data Posting = Posting Account Commodity (Maybe Lot) ValAmount (Maybe Tag) deriving (Show, Eq)

instance Ord Transaction where
  (Transaction d1 _ _ _) <= (Transaction d2 _ _ _) = d1 <= d2

instance Pretty Transaction where
  pretty (Transaction date description tags postings) =
    hsep ([pretty date, dquotes (pretty $ quote description)] ++ (pretty <$> tags))
      <> hardline
      <> vsep (pretty <$> postings)
    where
      quote :: Text -> Text
      quote = Text.replace "\"" "\\\""

instance Pretty Posting where
  pretty (Posting a c l amt t) =
    hsep $
      [ pretty a,
        pretty . ValAmount.showFixed $ amt,
        pretty c
      ]
        ++ [pretty l | Maybe.isJust l]
        ++ [pretty t | Maybe.isJust t]

newtype Tag = Tag Text deriving (Eq, Ord, Show)

instance Pretty Tag where
  pretty (Tag t) = pretty t

newtype UnnecessaryWildcard = UnnecessaryWildcard [Posting]
  deriving (Show)

instance Exception UnnecessaryWildcard

instance Pretty UnnecessaryWildcard where
  pretty (UnnecessaryWildcard p) = Pretty.pretty ("Unnecessary wildcard: " :: Text) <+> pretty p

createBalanced ::
  MonadThrow m =>
  Date ->
  Text ->
  [Tag] ->
  [Posting] ->
  Maybe Account ->
  m Transaction
createBalanced date desc tags postings wildcard = do
  balancePosting <- completePostings wildcard postings
  pure $ Transaction date desc tags (postings <> balancePosting)

createAdjustment :: MonadThrow m => Date -> (Position, ValAmount) -> m Transaction
createAdjustment date (Position account commodity lot, amount) = do
  let description = Text.pack ("Valuation " <> show commodity)
      valAccount = Account.valuationAccount
      postings =
        [ Posting account commodity lot amount Nothing,
          Posting valAccount commodity lot (invert amount) Nothing
        ]
  pure $ Transaction date description [] postings

completePostings :: MonadThrow m => Maybe Account -> [Posting] -> m [Posting]
completePostings wildcard postings = do
  let imbalance = calculateImbalance postings
      i = length imbalance
  case wildcard of
    Just w
      | i > 0 -> pure $ balanceImbalances w imbalance
      | otherwise -> pure []
    Nothing
      | i > 0 -> throwM $ UnnecessaryWildcard postings
      | otherwise -> pure []

calculateImbalance :: [Posting] -> [(Commodity, ValAmount)]
calculateImbalance = Map.toList . Map.filter (not . ValAmount.amountIsZero) . Map.fromListWith (<>) . fmap (\(Posting _ c _ a _) -> (c, a))

balanceImbalances :: Account -> [(Commodity, ValAmount)] -> [Posting]
balanceImbalances account = fmap (\(c, a) -> Posting account c Nothing (invert a) Nothing)

accounts :: Transaction -> [Account]
accounts (Transaction _ _ _ postings) = fmap (\(Posting a _ _ _ _) -> a) postings

valuate :: MonadThrow m => Commodity -> (Commodity -> Amount -> m Amount) -> Transaction -> m Transaction
valuate tc f (Transaction d desc tags postings) = do
  postings' <- for postings g
  pure $ Transaction d desc tags postings'
  where
    g (Posting a c l amt t) = Posting a c l <$> ValAmount.valuate tc (f c) amt <*> pure t

match :: Filter -> Transaction -> Bool
match (Filter af cf) (Transaction _ _ _ p) = any isMatch p
  where
    isMatch (Posting a c _ _ _) = Account.match af a && Commodity.match cf c
