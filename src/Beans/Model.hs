{-# LANGUAGE DeriveTraversable #-}
module Beans.Model
  ( Accounts
  , Amount
  , Amounts
  , AccountType(..)
  , Commodity(..)
  , Date(MinDate, MaxDate)
  , fromGreg
  , parseDate
  , Lot(..)
  , Filter(..)
  , Position(..)
  , balance
  , summarize
  , eraseLots
  , format
  , Account(..)
  , Command(..)
  , Dated(..)
  , between
  , Directive(..)
  , mkBalancedTransaction
  , Posting
  , Include(..)
  , Option(..)
  , Tag(..)
  , Flag(..)
  , Ledger
  , build
  , filter
  , Restrictions
  , Restriction(..)
  , isCompatible
  )
where



import           Prelude                 hiding ( filter )
import           Control.Monad.Catch            ( Exception
                                                , MonadThrow
                                                , throwM
                                                )
import qualified Text.Megaparsec.Pos           as P

import qualified Beans.Data.Map                as M
import           Data.Foldable                  ( fold )
import qualified Data.List                     as L
import           Data.Maybe                     ( catMaybes )
import           Data.Monoid                    ( Sum
                                                , getSum
                                                )
import           Text.Regex.PCRE                ( (=~) )
import           Data.Scientific                ( Scientific
                                                , formatScientific
                                                , FPFormat(Fixed)
                                                )
import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                )
import           Data.Time.Calendar             ( Day
                                                , fromGregorian
                                                )
import           Data.Time.Format               ( defaultTimeLocale
                                                , parseTimeM
                                                )

type Amount = Sum Scientific

format :: Amount -> Text
format = pack . formatScientific Fixed (Just 2) . getSum


data Date = MinDate | Date Day | MaxDate deriving (Eq, Ord)

fromGreg :: Integer -> Int -> Int -> Date
fromGreg y m d = Date $ fromGregorian y m d

parseDate :: String -> String -> Maybe Date
parseDate fmt input = Date <$> parseTimeM False defaultTimeLocale fmt input

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

summarize :: Maybe Int -> Accounts -> Accounts
summarize (Just d) = M.mapKeysM $ \p -> p { pAccount = shorten d (pAccount p) }
summarize Nothing  = id

shorten :: Int -> Account -> Account
shorten d (Account t a) = Account t (take d a)

eraseLots :: Bool -> Accounts -> Accounts
eraseLots False = M.mapKeysM (\p -> p { pLot = Nothing })
eraseLots True  = id


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

between :: Date -> Date -> Date -> Bool
between from to d = from <= d && d <= to

data Command
  = Open {
    oAccount :: Account
  , oRestriction :: Restriction
  }
  | Price {
    prCommodity :: Commodity
  , prPrice :: Scientific
  , prTargetCommodity :: Commodity
  }
  | Transaction {
    tFlag :: Flag
  , tDescription :: Text
  , tTags :: [Tag]
  , tPostings :: Accounts
  }
  | Balance {
    bAccount :: Account
  , bAmount :: Amount
  , bCommodity :: Commodity
  }
  | Close {
    cAccount :: Account
  }
  deriving (Eq, Show, Ord)


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
  -> m Command
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
calculateImbalances = M.filter (/= 0) . mconcat . fmap snd . M.toList

balanceImbalances :: Account -> M.Map Commodity Amount -> Accounts
balanceImbalances account = M.mapEntries g . fmap negate
 where
  g (commodity, amount) =
    (Position account commodity Nothing, M.singleton commodity amount)

-- Ledger

type Ledger = M.Map Date [Command]

build :: [Directive] -> Ledger
build d =
  let l = [ (dt, [co]) | DatedCommandDirective (Dated dt co) <- d ]
  in  L.sort <$> M.fromListM l

data Filter =
    NoFilter
  | StrictFilter String
  | Filter String
  | PeriodFilter Date Date deriving (Eq, Show)


filter :: Filter -> Ledger -> Ledger
filter (StrictFilter regex) =
  fmap (fmap (filterPostings regex)) . filter (Filter regex)
filter (Filter regex) =
  M.filter (/= mempty) . fmap (L.filter (matchCommand regex))
filter (PeriodFilter from to) = M.filterWithKey (const . between from to)
filter NoFilter               = id

filterPostings :: String -> Command -> Command
filterPostings regex Transaction { tPostings, ..} = Transaction
  { tPostings = M.filterKeys ((=~ regex) . show . pAccount) tPostings
  , ..
  }
filterPostings _ command = command

matchCommand :: String -> Command -> Bool
matchCommand regex Transaction { tPostings } = (any match . M.keys) tPostings
  where match = (=~ regex) . show . pAccount
matchCommand _ Balance {..} = False
matchCommand _ Price {..}   = False
matchCommand _ _            = True


-- Restrictions

type Restrictions = M.Map Account Restriction

data Restriction
  = NoRestriction
  | RestrictedTo [Commodity]
  deriving (Eq, Ord, Show)

instance Semigroup Restriction where
  RestrictedTo x <> RestrictedTo y = RestrictedTo (x `L.union` y)
  _ <> _ = NoRestriction

instance Monoid Restriction where
  mempty = RestrictedTo []
  RestrictedTo x `mappend` RestrictedTo y = RestrictedTo (x `L.union` y)
  _ `mappend` _ = NoRestriction

isCompatible :: Restriction -> Commodity -> Bool
isCompatible r c = case r of
  NoRestriction     -> True
  (RestrictedTo cs) -> c `elem` cs
