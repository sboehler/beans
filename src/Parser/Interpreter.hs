module Parser.Interpreter where

import Control.Lens ((%~), (.~), (^.), mapMOf, view)
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Data.Decimal (Decimal)
import Data.List ((\\))
import qualified Data.Map.Lazy as M
import Data.Maybe (mapMaybe)
import Parser.AST
import qualified Text.Parsec as P

type Weight = (CommodityName, Decimal)

type Weights = [Weight]

data HaricotException
  = NoWildcardPostings
  | TooManyWildcardPostings
  deriving (Eq, Show)

instance Exception HaricotException

completeTransaction ::
     (MonadThrow m) => Directive P.SourcePos -> m (Directive P.SourcePos)
completeTransaction (Trn t pos) = do
  balanced <- completePostings $ t ^. postings
  let t' = postings .~ balanced $ t
  return $ Trn t' pos
completeTransaction x = return x

completePostings :: (MonadThrow m) => [Posting] -> m [Posting]
completePostings p =
  case imbalances of
    [] -> return $ p \\ wildcards
    _ ->
      case wildcards of
        [WildcardPosting accountName] ->
          let balances = createBalanceBooking accountName <$> imbalances
          in return $ balances ++ (p \\ wildcards)
        [] -> throwM NoWildcardPostings
        _ -> throwM TooManyWildcardPostings
  where
    imbalances = calculateImbalances p
    wildcards = [w | w@WildcardPosting {..} <- p]

createBalanceBooking :: AccountName -> (CommodityName, Decimal) -> Posting
createBalanceBooking account (c, a) = CompletePosting account (-a) c [] Nothing

calculateImbalances :: [Posting] -> [(CommodityName, Decimal)]
calculateImbalances =
  M.toList . M.filter notZero . M.fromListWith (+) . mapMaybe calculateWeight

notZero :: Decimal -> Bool
notZero n = abs n > 0.005

calculateWeight :: Posting -> Maybe (CommodityName, Decimal)
calculateWeight WildcardPosting {..} = Nothing
calculateWeight CompletePosting {..} =
  case findCost _postingCost of
    Just (c, a) -> Just (c, _amount * a)
    Nothing ->
      case _postingPrice of
        Just UnitPrice {..} ->
          Just (_unitPriceCommodity, _amount * _unitPriceAmount)
        Just TotalPrice {..} ->
          Just (_totalPriceCommodity, signum _amount * _totalPriceAmount)
        Nothing -> Just (_postingCommodityName, _amount)

findCost :: [PostingCost] -> Maybe (CommodityName, Decimal)
findCost (PostingCostAmount {..}:_) =
  Just (_postingCostCommmodity, _postingCostAmount)
findCost (_:costs) = findCost costs
findCost [] = Nothing
