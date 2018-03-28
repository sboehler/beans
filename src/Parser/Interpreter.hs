module Parser.Interpreter
  ( completePostings
  ) where

import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Data.Account (AccountName)
import Data.Amount (Amount(..))
import Data.Commodity (CommodityName)
import Data.Lot (Lot(..))
import Data.Scientific(Scientific)
import qualified Data.Map.Lazy as M
import Data.Posting (Posting(..), PostingPrice(..))
import Parser.AST (PostingDirective(..))

data HaricotException =
  UnbalancedTransaction
  deriving (Eq, Show)

instance Exception HaricotException

completePostings :: (MonadThrow m) => [PostingDirective] -> m [Posting]
completePostings p =
  case (calculateImbalances postings, wildcardAccount) of
    ([], []) -> return postings
    (im, [account]) -> return $ postings ++ (uncurry (balance account) <$> im)
    _ -> throwM UnbalancedTransaction
  where
    wildcardAccount = [n | WildcardPosting n <- p]
    postings = [p' | CompletePosting p' <- p]

balance :: AccountName -> CommodityName -> Scientific -> Posting 
balance account commodity amount =
  Posting account (negate amount) commodity Nothing Nothing

calculateImbalances :: [Posting ] -> [(CommodityName, Scientific)]
calculateImbalances =
  M.toList . M.filter ((> 0.005) . abs) . M.fromListWith (+) . fmap weight

weight :: Posting  -> (CommodityName, Scientific)
weight Posting {_lot = Just (Lot (Amount p c) _ _), ..} = (c, _amount * p)
weight Posting {_price = Just (UnitPrice (Amount p c)), ..} = (c, _amount * p)
weight Posting {_price = Just (TotalPrice (Amount a c)), ..} =
  (c, signum _amount * a)
weight Posting {..} = (_commodity, _amount)
