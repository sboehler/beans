module Beans.Data.Accounts
  ( Accounts
  , Amount
  , Amounts
  , AccountType(..)
  , Commodity(..)
  , Date(..)
  , Lot(..)
  , balance
  , summarize
  , eraseLots
  , Account(..)
  )
where

import qualified Beans.Data.Map                as M
import           Data.Foldable                            ( fold )
import qualified Data.List                     as L
import           Data.Maybe                               ( catMaybes )
import           Data.Monoid                              ( Sum )
import           Data.Scientific                          ( Scientific )
import           Data.Text                                ( Text
                                                          , unpack
                                                          )
import           Data.Time.Calendar                       ( Day )

type Amount = Sum Scientific

data Date = MinDate | Date Day | MaxDate deriving (Eq, Ord)

instance Show Date where
  show MinDate = "<MIN_DATE>"
  show MaxDate = "<MAX_DATE>"
  show (Date d) = show d

type Amounts = M.Map Commodity Amount

type Accounts = M.Map (Account, Commodity, Maybe Lot) Amounts

data AccountType
  = Assets
  | Liabilities
  | Equity
  | Income
  | Expenses
  deriving (Eq, Ord, Read, Show)

data Account =
  Account AccountType
              [Text]
  deriving (Eq, Ord)

instance Show Account where
  show (Account t n) = L.intercalate ":" (show t : (unpack <$> n))

newtype Commodity =
  Commodity Text
  deriving (Eq, Ord)

instance Show Commodity where
  show (Commodity n) = unpack n

data Lot = Lot
  { lPrice           :: Amount
  , lTargetCommodity :: Commodity
  , lDate            :: Date
  , lLabel           :: Maybe Text
  } deriving (Eq, Ord)

instance Show Lot where
  show Lot {lPrice, lTargetCommodity, lDate, lLabel} =
    let price = show lPrice ++ " " ++ show lTargetCommodity
        elems = catMaybes [Just price, Just $ show lDate, show <$> lLabel]
     in "{ " ++ L.intercalate ", " elems ++ " }"

balance :: Account -> Commodity -> Accounts -> Amount
balance accountName commodityName =
  M.findWithDefaultM commodityName . fold . M.filterWithKey f
  where f (a, c, _) _ = accountName == a && commodityName == c

summarize :: Int -> Accounts -> Accounts
summarize d = M.mapKeysM g where g (a, c, l) = (shorten d a, c, l)

shorten :: Int -> Account -> Account
shorten d (Account t a) = Account t (take d a)

eraseLots :: Accounts -> Accounts
eraseLots = M.mapKeysM g where g (a, c, _) = (a, c, Nothing)
