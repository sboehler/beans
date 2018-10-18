module Beans.Data.Accounts
  ( Accounts
  , Amount
  , Amounts
  , AccountType(..)
  , Commodity(..)
  , Date(..)
  , Lot(..)
  , Position(..)
  , balance
  , summarize
  , eraseLots
  , format
  , Account(..)
  )
where

import qualified Beans.Data.Map                as M
import           Data.Foldable                            ( fold )
import qualified Data.List                     as L
import           Data.Maybe                               ( catMaybes )
import           Data.Monoid                              ( Sum
                                                          , getSum
                                                          )
import           Data.Scientific                          ( Scientific
                                                          , formatScientific
                                                          , FPFormat(Fixed)
                                                          )
import           Data.Text                                ( Text
                                                          , unpack
                                                          , pack
                                                          )
import           Data.Time.Calendar                       ( Day )

type Amount = Sum Scientific

format :: Amount -> Text
format = pack . formatScientific Fixed (Just 2) . getSum


data Date = MinDate | Date Day | MaxDate deriving (Eq, Ord)

instance Show Date where
  show MinDate = "<MIN_DATE>"
  show MaxDate = "<MAX_DATE>"
  show (Date d) = show d

type Amounts = M.Map Commodity Amount

data Position = Position {
  pAccount :: Account,
  pCommodity :: Commodity,
  pLot :: Maybe Lot
  }
  deriving (Eq, Ord, Show)

type Accounts = M.Map Position Amounts

data AccountType
  = Assets
  | Liabilities
  | Equity
  | Income
  | Expenses
  deriving (Eq, Ord, Read, Show)

data Account =
  Account {
  aType :: AccountType,
  aSegments :: [Text]
  }
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
  M.findWithDefaultM commodityName . fold . M.filterKeys f
 where
  f Position { pAccount, pCommodity } =
    accountName == pAccount && commodityName == pCommodity

summarize :: Int -> Accounts -> Accounts
summarize d = M.mapKeysM $ \p -> p { pAccount = shorten d (pAccount p) }

shorten :: Int -> Account -> Account
shorten d (Account t a) = Account t (take d a)

eraseLots :: Accounts -> Accounts
eraseLots = M.mapKeysM (\p -> p { pLot = Nothing })
