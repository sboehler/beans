module Parser.Interpreter where

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Data.Decimal (Decimal)
import Data.Foldable (asum)
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

summarize :: [Posting] -> [(CommodityName, Decimal)]
summarize =
  M.toList . M.filter notZero . M.fromListWith (+) . mapMaybe calculateWeight

balance :: AccountName -> [(CommodityName, Decimal)] -> [Posting]
balance account = map (\(c, a) -> CompletePosting account (-a) c [] Nothing)

notZero :: Decimal -> Bool
notZero n = abs n > 0.005

calculateWeight :: Posting -> Maybe (CommodityName, Decimal)
calculateWeight posting =
  atCost posting <|> atPrice posting <|> asBooked posting

atCost :: Posting -> Maybe (CommodityName, Decimal)
atCost CompletePosting {..} = asum (f <$> _postingCost)
  where
    f (PostingCostAmount a c) = Just (c, _amount * a)
    f _ = Nothing
atCost _ = Nothing

atPrice :: Posting -> Maybe (CommodityName, Decimal)
atPrice CompletePosting {..} = f <$> _postingPrice
  where
    f (UnitPrice a c) = (c, _amount * a)
    f (TotalPrice a c) = (c, signum _amount * a)
atPrice _ = Nothing

asBooked :: Posting -> Maybe (CommodityName, Decimal)
asBooked CompletePosting {..} = Just (_postingCommodityName, _amount)
asBooked _ = Nothing
