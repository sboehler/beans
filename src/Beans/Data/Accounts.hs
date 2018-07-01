module Beans.Data.Accounts where

import qualified Beans.Data.Map     as M
import           Data.Foldable      (fold)
import           Data.Group         (invert)
import qualified Data.List          as L
import           Data.Maybe         (catMaybes)
import           Data.Monoid        (Sum (Sum), getSum)
import           Data.Scientific    (Scientific)
import           Data.Text.Lazy     (Text, unpack)
import           Data.Time.Calendar (Day)

type Accounts = M.Map (AccountName, CommodityName, Maybe Lot) (Sum Scientific)

data AccountType
  = Assets
  | Liabilities
  | Equity
  | Income
  | Expenses
  deriving (Eq, Ord, Read, Show)

data AccountName = AccountName
  { _unAccountType :: AccountType
  , _unAccountName :: [Text]
  } deriving (Eq, Ord)

instance Show AccountName where
  show (AccountName t n) = L.intercalate ":" (show t : (unpack <$> n))

newtype CommodityName = CommodityName
  { _unCommodityName :: Text
  } deriving (Eq, Ord)

instance Show CommodityName where
  show (CommodityName n) = unpack n

data Lot
  = Lot { _price           :: Scientific
        , _targetCommodity :: CommodityName
        , _date            :: Day
        , _label           :: Maybe Text }
  deriving (Eq, Ord)

instance Show Lot where
  show (Lot p t d l) =
    let price = show p ++ " " ++ show t
        elems = catMaybes [Just price, Just $ show d, show <$> l]
     in "{ " ++ L.intercalate ", " elems ++ " }"


add :: AccountName -> CommodityName -> Maybe Lot -> Scientific -> Accounts -> Accounts
add a c l s = M.insert (a, c, l) (mappend (Sum s))

minus :: Accounts -> Accounts -> Accounts
minus a1 a2 = a1 `mappend` invert a2

mapAccounts :: (AccountName -> AccountName) -> Accounts -> Accounts
mapAccounts f = M.mapKeys (\(a, c, l) -> (f a, c, l))

mapLots :: (Maybe Lot -> Maybe Lot) -> Accounts -> Accounts
mapLots f = M.mapKeys (\(a, c, l) -> (a, c, f l))

balance :: AccountName -> CommodityName -> Accounts -> Scientific
balance a c = getSum . fold . filter'
  where
    filter' = M.filterByKey (\(a', c', _) -> a' == a && c' == c)

filter :: (Scientific -> Bool) -> Accounts -> Accounts
filter f = M.filter (f . getSum)

split :: AccountName -> Accounts -> (Accounts, Accounts)
split a = M.split (\(a', _, _) -> (a == a'))

toList ::
     Accounts -> [((AccountName, CommodityName, Maybe Lot), Scientific)]
toList = M.toList . fmap getSum
