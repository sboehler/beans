module Haricot.Verify (completeDirectives) where

import           Control.Monad.Catch
import qualified Data.Map.Strict     as M
import           Data.Scientific     (Scientific)
import           Haricot.AST
import qualified Text.Megaparsec.Pos as P

newtype VerifyException =
  UnbalancedTransaction P.SourcePos
  deriving (Eq, Show)

instance Exception VerifyException

completeDirectives :: MonadThrow m => [Directive [Posting]] -> m [Directive [CompletePosting]]
completeDirectives = mapM (traverse completePostings)

completePostings :: (MonadThrow m) => [Posting] -> m [CompletePosting]
completePostings p =
  let wildcards = [w | WP w <- p]
      completes = [c | CP c <- p]
   in case calculateImbalances completes of
        [] -> return completes
        imbalances ->
          case wildcards of
            [w] -> return $ completes ++ map (balance w) imbalances
            _ -> throwM $ UnbalancedTransaction (pos $ head completes)
              where pos (CompletePosting {_pos}) = _pos

balance :: WildcardPosting -> (CommodityName, Scientific) -> CompletePosting
balance WildcardPosting {_pos, _account} (commodity, amount) =
  CompletePosting
    {_amount = negate amount, _commodity = commodity, _lot = Nothing, ..}

calculateImbalances :: [CompletePosting] -> [(CommodityName, Scientific)]
calculateImbalances =
  M.toList . M.filter ((> 0.005) . abs) . M.fromListWith (+) . fmap weight

weight :: CompletePosting  -> (CommodityName, Scientific)
weight CompletePosting {..} =
  case _lot of
    Just Lot {_price, _targetCommodity} ->
      (_targetCommodity, _amount * _price)
    _ -> (_commodity, _amount)
