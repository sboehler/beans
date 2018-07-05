module Beans.Data.Accounts where

import qualified Beans.Data.Map     as M
import           Data.Foldable      (fold)
import           Data.Group         (invert)
import qualified Data.List          as L
import           Data.Maybe         (catMaybes)
import           Data.Monoid        (Sum)
import           Data.Scientific    (Scientific)
import           Data.Text          (Text, unpack)
import           Data.Time.Calendar (Day)

type Amount = Sum Scientific

type Accounts = M.Map (AccountName, CommodityName, Maybe Lot) Amount

type AccountsHistory = M.Map Day Accounts

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
  = Lot { _price           :: Amount
        , _targetCommodity :: CommodityName
        , _date            :: Day
        , _label           :: Maybe Text }
  deriving (Eq, Ord)

instance Show Lot where
  show (Lot p t d l) =
    let price = show p ++ " " ++ show t
        elems = catMaybes [Just price, Just $ show d, show <$> l]
     in "{ " ++ L.intercalate ", " elems ++ " }"


add :: AccountName -> CommodityName -> Maybe Lot -> Amount -> Accounts -> Accounts
add a c l s = M.insert (a, c, l) (mappend s)

minus :: Accounts -> Accounts -> Accounts
minus a1 a2 = a1 `mappend` invert a2

mapAccounts :: (AccountName -> AccountName) -> Accounts -> Accounts
mapAccounts f = M.mapKeys (\(a, c, l) -> (f a, c, l))

mapLots :: (Maybe Lot -> Maybe Lot) -> Accounts -> Accounts
mapLots f = M.mapKeys (\(a, c, l) -> (a, c, f l))

balance :: AccountName -> CommodityName -> Accounts -> Amount
balance a c = fold . filter'
  where
    filter' = M.filterByKey (\(a', c', _) -> a' == a && c' == c)

filter :: (Amount -> Bool) -> Accounts -> Accounts
filter = M.filter 

split :: AccountName -> Accounts -> (Accounts, Accounts)
split a = M.split (\(a', _, _) -> (a == a'))

toList ::
     Accounts -> [((AccountName, CommodityName, Maybe Lot), Amount)]
toList = M.toList 

lookupLE :: forall v k. (Ord k, Monoid v) => k -> M.Map k v -> v
lookupLE = M.lookupLE

lookupLT :: forall v k. (Ord k, Monoid v) => k -> M.Map k v -> v
lookupLT = M.lookupLT

summarize :: Int -> Accounts -> Accounts
summarize d = mapAccounts (shorten d)

shorten :: Int -> AccountName -> AccountName
shorten d a = a {_unAccountName = take d (_unAccountName a)}

eraseLots :: Accounts -> Accounts
eraseLots = mapLots (const Nothing)
