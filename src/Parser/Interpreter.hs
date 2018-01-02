module Parser.Interpreter where

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad (join)
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Data.Accounts (AccountName)
import Data.Amount (Amount(..))
import qualified Data.Amount as A
import Data.Commodity (CommodityName)
import Data.Decimal (Decimal)
import Data.Foldable (asum)
import qualified Data.Holdings as H
import qualified Data.Map.Lazy as M
import Data.Maybe (mapMaybe)
import Parser.AST
import qualified Text.Parsec as P

type Weight = (CommodityName, Decimal)

type Weights = [Weight]

data HaricotException =
  UnbalancedTransaction
  deriving (Eq, Show)

instance Exception HaricotException

completeTransaction ::
     (MonadThrow m) => Directive P.SourcePos -> m (Directive P.SourcePos)
completeTransaction = mapMOf (_Trn . _1 . postings) completePostings

completePostings :: (MonadThrow m) => [Posting] -> m [Posting]
completePostings p =
  case (summarize p, wildcardAccounts) of
    ([], []) -> return p
    (imbalances, [account]) -> return $ complete ++ balance account imbalances
    (_, _) -> throwM UnbalancedTransaction
  where
    wildcardAccounts = [_postingAccountName | WildcardPosting {..} <- p]
    complete = [x | x@CompletePosting {..} <- p]

summarize :: [Posting] -> [Amount Decimal]
summarize = H.toList . H.filter notZero . H.fromList . mapMaybe calculateWeight

balance :: AccountName -> [Amount Decimal] -> [Posting]
balance account = fmap $ \a -> CompletePosting account (negate <$> a) [] Nothing

notZero :: Decimal -> Bool
notZero n = abs n > 0.005

calculateWeight :: Posting -> Maybe (Amount Decimal)
calculateWeight posting =
  atCost posting <|> atPrice posting <|> asBooked posting

atCost :: Posting -> Maybe (Amount Decimal)
atCost CompletePosting {..} = asum . fmap f $ _postingCost
  where
    f (PostingCostAmount a c) = Just $ Amount (A._amount _amount * a) c
    f _ = Nothing
atCost _ = Nothing

atPrice :: Posting -> Maybe (Amount Decimal)
atPrice p = f <$> preview amount p <*> join (preview postingPrice p)
  where
    f (Amount a _) (UnitPrice a' c') = Amount (a * a') c'
    f (Amount a _) (TotalPrice a' c') = Amount (signum a * a') c'

asBooked :: Posting -> Maybe (Amount Decimal)
asBooked = preview amount
