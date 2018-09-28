module Beans.Data.Accounts
  ( Accounts
  , Amount
  , Amounts
  , AccountsHistory
  , AccountType(..)
  , CommodityName(..)
  , Lot(..)
  , balance
  , summarize
  , eraseLots
  , AccountName(..)
  ) where

import qualified Beans.Data.Map     as M
import           Data.Foldable      (fold)
import qualified Data.List          as L
import           Data.Maybe         (catMaybes)
import           Data.Monoid        (Sum)
import           Data.Scientific    (Scientific)
import           Data.Text          (Text, unpack)
import           Data.Time.Calendar (Day)

type Amount = Sum Scientific

type Amounts = M.Map CommodityName Amount

type Accounts = M.Map (AccountName, CommodityName, Maybe Lot) Amounts

type AccountsHistory = M.Map Day Accounts

data AccountType
  = Assets
  | Liabilities
  | Equity
  | Income
  | Expenses
  deriving (Eq, Ord, Read, Show)

data AccountName =
  AccountName AccountType
              [Text]
  deriving (Eq, Ord)

instance Show AccountName where
  show (AccountName t n) = L.intercalate ":" (show t : (unpack <$> n))

newtype CommodityName =
  CommodityName Text
  deriving (Eq, Ord)

instance Show CommodityName where
  show (CommodityName n) = unpack n

data Lot = Lot
  { lPrice           :: Amount
  , lTargetCommodity :: CommodityName
  , lDate            :: Day
  , lLabel           :: Maybe Text
  } deriving (Eq, Ord)

instance Show Lot where
  show Lot {lPrice, lTargetCommodity, lDate, lLabel} =
    let price = show lPrice ++ " " ++ show lTargetCommodity
        elems = catMaybes [Just price, Just $ show lDate, show <$> lLabel]
     in "{ " ++ L.intercalate ", " elems ++ " }"

balance :: AccountName -> CommodityName -> Accounts -> Amount
balance accountName commodityName =
  M.findWithDefaultM commodityName . fold . M.filterWithKey f
  where f (a, c, _) _ = accountName == a && commodityName == c

summarize :: Int -> Accounts -> Accounts
summarize d = M.mapKeysM g where g (a, c, l) = (shorten d a, c, l)

shorten :: Int -> AccountName -> AccountName
shorten d (AccountName t a) = AccountName t (take d a)

eraseLots :: Accounts -> Accounts
eraseLots = M.mapKeysM g where g (a, c, _) = (a, c, Nothing)
