module Parser.Interpreter
  ( completePostings
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Data.Accounts (AccountName)
import Data.Amount (Amount(..))
import Data.Decimal (Decimal)
import Data.Foldable (asum)
import qualified Data.Holdings as H
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Price ((<@>))
import Parser.AST
       (Posting(..), PostingCost(..), PostingDirective(..),
        PostingPrice(..))

data HaricotException =
  UnbalancedTransaction
  deriving (Eq, Show)

instance Exception HaricotException

completePostings :: (MonadThrow m) => [PostingDirective] -> m [Posting]
completePostings p =
  case (imbalances, wildcardAccount) of
    ([], Nothing) -> return postings
    (_, Just account) -> return (postings ++ balance account imbalances)
    _ -> throwM UnbalancedTransaction
  where
    wildcardAccount = listToMaybe [n | WildcardPosting n <- p]
    postings = [p' | CompletePosting p' <- p]
    imbalances = calculateImbalances postings

calculateImbalances :: [Posting] -> [Amount Decimal]
calculateImbalances =
  H.toList . H.filter notZero . H.fromList . mapMaybe calculateWeight

balance :: (Functor f) => AccountName -> f (Amount Decimal) -> f Posting
balance account = fmap (\a -> Posting account (negate <$> a) [] Nothing)

notZero :: Decimal -> Bool
notZero n = abs n > 0.005

calculateWeight :: Posting -> Maybe (Amount Decimal)
calculateWeight posting@Posting {..} =
  atCost posting <|> atPrice posting <|> Just _amount

atCost :: Posting -> Maybe (Amount Decimal)
atCost Posting {..} = asum $ f <$> _postingCost
  where
    f (PostingCostAmount p) = Just $ _amount <@> p
    f _ = Nothing

atPrice :: Posting -> Maybe (Amount Decimal)
atPrice (Posting _ a _ (Just (UnitPrice pr))) = Just $ a <@> pr
atPrice (Posting _ (Amount a _) _ (Just (TotalAmount ta))) =
  Just $ (signum a *) <$> ta
atPrice _ = Nothing
