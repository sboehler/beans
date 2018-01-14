module Parser.Interpreter
  ( completeTransaction
  ) where

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad (join)
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Data.Accounts (AccountName)
import Data.Amount (Amount(..))
import Data.Decimal (Decimal)
import Data.Foldable (asum)
import qualified Data.Holdings as H
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Price ((<@>))
import Parser.AST
import qualified Text.Parsec as P

data HaricotException =
  UnbalancedTransaction
  deriving (Eq, Show)

instance Exception HaricotException

completeTransaction ::
     (MonadThrow m) => Directive P.SourcePos -> m (Directive P.SourcePos)
completeTransaction = mapMOf (_Trn . _1 . postings) completePostings

completePostings :: (MonadThrow m) => [Posting] -> m [Posting]
completePostings p =
  case (imbalances, wildcardAccount) of
    ([], Nothing) -> return postings
    (_, Just account) -> return $ postings ++ balance account imbalances
    _ -> throwM UnbalancedTransaction
  where
    wildcardAccount =
      listToMaybe [_postingAccountName | WildcardPosting {..} <- p]
    postings = [c | c@CompletePosting {..} <- p]
    imbalances = calculateImbalances postings

calculateImbalances :: [Posting] -> [Amount Decimal]
calculateImbalances =
  H.toList . H.filter notZero . H.fromList . mapMaybe calculateWeight

balance :: AccountName -> [Amount Decimal] -> [Posting]
balance account = map (\a -> CompletePosting account (negate <$> a) [] Nothing)

notZero :: Decimal -> Bool
notZero n = abs n > 0.005

calculateWeight :: Posting -> Maybe (Amount Decimal)
calculateWeight posting =
  atCost posting <|> atPrice posting <|> asBooked posting

atCost :: Posting -> Maybe (Amount Decimal)
atCost CompletePosting {..} = asum $ f <$> _postingCost
  where
    f (PostingCostAmount p) = Just $ _amount <@> p
    f _ = Nothing
atCost _ = Nothing

atPrice :: Posting -> Maybe (Amount Decimal)
atPrice p = f <$> preview amount p <*> join (preview postingPrice p)
  where
    f am (UnitPrice pr) = am <@> pr
    f (Amount a _) (TotalAmount amt) = (signum a *) <$> amt

asBooked :: Posting -> Maybe (Amount Decimal)
asBooked = preview amount
