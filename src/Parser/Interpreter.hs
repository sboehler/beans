module Parser.Interpreter where

import Data.Decimal (Decimal)
import Data.List ((\\))
import Data.Map.Lazy (Map, foldlWithKey)
import qualified Data.Map.Lazy as M
import Data.Maybe (isJust, mapMaybe)
import Parser.AST

type Weight = (CommodityName, Decimal)

type Weights = [Weight]

completeTransaction :: DatedDirective -> DatedDirective
completeTransaction t@(Transaction _ _ _ postings) = t {_postings = postings}
completeTransaction x = x

completePostings :: [Posting] -> Either String [Posting]
completePostings p = do
  let w = M.filter (/= 0) $ weights p
      e = filter (isJust . _postingAmount) p
  case (length e, null w) of
    (0, True) -> Right p
    (0, False) -> Left "No posting without amount"
    (1, True) -> Right $ p \\ e
    (1, False) ->
      Right $
      foldlWithKey (balance (_accountName (head e :: Posting))) (p \\ e) w
    _ -> Left "Too many empty postings"

balance :: AccountName -> ([Posting] -> CommodityName -> Decimal -> [Posting])
balance account l c a =
  Posting account (Just (PostingAmount (Amount (-a) c) [] Nothing)) : l

weights :: [Posting] -> Map CommodityName Decimal
weights p = M.fromListWith (+) (mapMaybe weight p)
  where
    weight (Posting _ (Just a)) = Just $ calculateWeight a
    weight (Posting _ Nothing) = Nothing

calculateWeight :: PostingAmount -> (CommodityName, Decimal)
calculateWeight (PostingAmount amount' costs' price') = w amount' costs' price'
  where
    w (Amount a c) [] Nothing = (c, a)
    w (Amount a _) (PostingCostAmount (Amount a' c'):_) _ = (c', a' * a)
    w amount (_:costs) price = w amount costs price
    w (Amount a _) [] (Just (UnitPrice (Amount a' c'))) = (c', a' * a)
    w _ [] (Just (TotalPrice (Amount a c))) = (c, a)
