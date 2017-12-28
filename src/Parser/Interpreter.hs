module Parser.Interpreter where

import Data.Decimal (Decimal)
import Data.List ((\\))
import qualified Data.Map.Lazy as M
import Data.Maybe (mapMaybe)
import Parser.AST
import qualified Text.Parsec as P
import Text.Parsec.Error (Message(..), newErrorMessage)

type Weight = (CommodityName, Decimal)

type Weights = [Weight]

completeTransaction ::
     Directive P.SourcePos -> Either P.ParseError (Directive P.SourcePos)
completeTransaction (Trn t@Transaction {..} pos) =
  case completePostings _postings of
    Left s -> Left $ newErrorMessage (Message s) pos
    Right p -> Right $ Trn t {_postings = p} pos
completeTransaction x = Right x

completePostings :: [Posting] -> Either String [Posting]
completePostings p =
  case imbalances of
    [] -> Right $ p \\ wildcards
    _ ->
      case wildcards of
        [WildcardPosting accountName] ->
          let balances = createBalanceBooking accountName <$> imbalances
          in Right $ balances ++ (p \\ wildcards)
        [] -> Left "Error: No wildcard posting"
        _ -> Left "Error: Too many wildcard postings"
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
