module Parser.Interpreter
  ( completePostings
  ) where

import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Data.Account (AccountName)
import Data.Amount (Amount(..))
import Data.Commodity (CommodityName)
import Data.Decimal (Decimal)
import qualified Data.Map.Lazy as M
import Data.Posting (Posting(..), PostingPrice(..))
import Data.Price (Price(..))
import Parser.AST (PostingDirective(..))

data HaricotException =
  UnbalancedTransaction
  deriving (Eq, Show)

instance Exception HaricotException

completePostings :: (MonadThrow m) => [PostingDirective] -> m [Posting Decimal]
completePostings p =
  case (calculateImbalances postings, wildcardAccount) of
    ([], []) -> return postings
    (im, [account]) -> return $ postings ++ (balance account <$> im)
    _ -> throwM UnbalancedTransaction
  where
    wildcardAccount = [n | WildcardPosting n <- p]
    postings = [p' | CompletePosting p' <- p]

balance :: (Num a) => AccountName -> (CommodityName, a) -> Posting a
balance account (commodity, amount) =
  Posting account (negate amount) commodity Nothing Nothing Nothing Nothing

calculateImbalances ::
     (Ord a, Fractional a) => [Posting a] -> [(CommodityName, a)]
calculateImbalances =
  M.toList . M.filter ((> 0.005) . abs) . M.fromList . fmap weight

weight :: Num a => Posting a -> (CommodityName, a)
weight Posting {..} =
  case _lotCost of
    Just (Price a ct _) -> (ct, a * _amount)
    Nothing ->
      case _price of
        (Just (UnitPrice (Price a ct _))) -> (ct, a * _amount)
        (Just (TotalPrice (Amount a c))) -> (c, signum _amount * a)
        _ -> (_commodity, _amount)
