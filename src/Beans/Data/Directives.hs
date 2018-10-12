{-# Language DeriveTraversable #-}
module Beans.Data.Directives
  ( Command(..)
  , Dated(..)
  , Directive(..)
  , Transaction(..)
  , mkBalancedTransaction
  , Balance(..)
  , Posting
  , Open(..)
  , Close(..)
  , Include(..)
  , Option(..)
  , Price(..)
  , Tag(..)
  , Flag(..)
  )
where

import           Beans.Data.Accounts                      ( Account
                                                          , Accounts
                                                          , Date
                                                          , Amount
                                                          , Amounts
                                                          , Commodity
                                                          , Lot(..)
                                                          )
import qualified Beans.Data.Map                as M
import           Beans.Data.Restrictions                  ( Restriction )
import           Control.Monad.Catch                      ( Exception
                                                          , MonadThrow
                                                          , throwM
                                                          )
import           Data.Scientific                          ( Scientific )
import           Data.Text                                ( Text )
import qualified Text.Megaparsec.Pos           as P

data Directive
  = DatedCommandDirective (Dated Command)
  | OptionDirective Option
  | IncludeDirective Include
  deriving (Eq, Ord, Show)

data Dated a =
  Dated {
    date :: Date,
    undate :: a
    }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Command
  = BalanceCommand Balance
  | OpenCommand Open
  | CloseCommand Close
  | TransactionCommand Transaction
  | PriceCommand Price
  deriving (Eq, Ord, Show)

data Balance = Balance
  { bAccount   :: Account
  , bAmount    :: Amount
  , bCommodity :: Commodity
  } deriving (Eq, Ord, Show)

data Open = Open
  { oAccount     :: Account
  , oRestriction :: Restriction
  } deriving (Eq, Ord, Show)

newtype Close = Close
  { cAccount :: Account
  } deriving (Eq, Ord, Show)

data Price = Price
  { pCommodity       :: Commodity
  , pPrice           :: Scientific
  , pTargetCommodity :: Commodity
  } deriving (Eq, Ord, Show)

data Transaction = Transaction
  { tFlag        :: Flag
  , tDescription :: Text
  , tTags        :: [Tag]
  , tPostings    :: Accounts
  } deriving (Eq, Show)

instance Ord Transaction where
  _ `compare` _ = EQ

data Flag
  = Complete
  | Incomplete
  deriving (Eq, Ord, Show)

newtype Tag =
  Tag Text
  deriving (Eq, Ord, Show)

data Include = Include
  { iPos      :: P.SourcePos
  , iFilePath :: FilePath
  } deriving (Eq, Ord, Show)

data Option =
  Option P.SourcePos
         Text
         Text
  deriving (Eq, Ord, Show)

data UnbalancedTransaction =
  UnbalancedTransaction Accounts
                        (M.Map Commodity Amount)
  deriving (Eq, Show)

type Posting = ((Account, Commodity, Maybe Lot), Amounts)

instance Exception UnbalancedTransaction

mkBalancedTransaction
  :: MonadThrow m
  => Flag
  -> Text
  -> [Tag]
  -> Accounts
  -> Maybe Account
  -> m Transaction
mkBalancedTransaction flag desc tags postings wildcard =
  Transaction flag desc tags <$> completePostings wildcard postings

completePostings :: MonadThrow m => Maybe Account -> Accounts -> m Accounts
completePostings wildcard postings =
  mappend postings <$> fixImbalances wildcard imbalances
 where
  imbalances = calculateImbalances postings
  fixImbalances w i
    | null i = return mempty
    | M.size i == 1 = case w of
      Just account -> return $ balanceImbalances account i
      Nothing      -> throwM $ UnbalancedTransaction postings i
    | otherwise = return mempty

calculateImbalances :: Accounts -> M.Map Commodity Amount
calculateImbalances = mconcat . fmap snd . M.toList

balanceImbalances :: Account -> M.Map Commodity Amount -> Accounts
balanceImbalances account = M.mapEntries g . fmap negate
  where g (c, s) = ((account, c, Nothing), M.singleton c s)
