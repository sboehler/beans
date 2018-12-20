{-# LANGUAGE DeriveTraversable #-}

module Beans.Model where

import           Control.Monad.Catch            ( Exception
                                                , MonadThrow
                                                , throwM
                                                )
import           Prelude                 hiding ( filter )
import qualified Text.Megaparsec.Pos           as P
import qualified Beans.Data.Map                as M
import           Control.Lens
import           Data.Foldable                  ( fold )
import qualified Data.List                     as L
import           Data.Maybe                     ( catMaybes )
import           Data.Monoid                    ( Sum(Sum)
                                                , getSum
                                                )
import qualified Data.Decimal                  as D
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.Time.Calendar             ( Day
                                                , fromGregorian
                                                )
import           Text.Regex.PCRE                ( (=~) )

type Amount = Sum D.Decimal

format :: Amount -> Text
format = pack . show . D.roundTo 2 . getSum

realFracToAmount :: RealFrac n => n -> Amount
realFracToAmount = Sum . D.realFracToDecimal 4

data Date
  = MinDate
  | Date Day
  | MaxDate
  deriving (Eq, Ord)

fromGreg :: Integer -> Int -> Int -> Date
fromGreg y m d = Date $ fromGregorian y m d

instance Show Date where
  show MinDate  = "<MIN_DATE>"
  show MaxDate  = "<MAX_DATE>"
  show (Date d) = show d

type Amounts = M.Map Commodity Amount

data Position = Position
  { _positionAccount :: Account
  , _positionCommodity :: Commodity
  , _positionLot :: Maybe Lot
  } deriving (Eq, Ord, Show)

type Accounts = M.Map Position Amounts

data AccountType
  = Assets
  | Liabilities
  | Equity
  | Income
  | Expenses
  deriving (Eq, Ord, Read, Show)

data Account = Account
  { _accountAccountType :: AccountType
  , _accountSegments :: [Text]
  } deriving (Eq, Ord)

instance Show Account where
  show (Account t n) = L.intercalate ":" (show t : (unpack <$> n))

newtype Commodity =
  Commodity Text
  deriving (Eq, Ord)

instance Show Commodity where
  show (Commodity n) = unpack n

data Lot = Lot
  { _lotPrice :: Amount
  , _lotTargetCommodity :: Commodity
  , _lotDate :: Date
  , _lotLabel :: Maybe Text
  } deriving (Eq, Ord)

instance Show Lot where
  show Lot { _lotPrice, _lotTargetCommodity, _lotDate, _lotLabel } =
    let price = show _lotPrice ++ " " ++ show _lotTargetCommodity
        elems =
          catMaybes [Just price, Just $ show _lotDate, show <$> _lotLabel]
    in  "{ " ++ L.intercalate ", " elems ++ " }"


data Directive
  = DatedCommandDirective (Dated Command)
  | OptionDirective Option
  | IncludeDirective Include
  deriving (Eq, Ord, Show)

data Dated a = Dated
  { date :: Date
  , undate :: a
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

between :: Date -> Date -> Date -> Bool
between dateFrom dateTo d = dateFrom <= d && d <= dateTo

data Command
  = CmdOpen Open
  | CmdPrice Price
  | CmdTransaction Transaction
  | CmdBalance Balance
  | CmdClose Close
  deriving (Eq, Show, Ord)

data Open = Open
  { _openAccount :: Account
  , _openRestriction :: Restriction
  } deriving (Eq, Show, Ord)

data Price = Price
  { _priceCommodity :: Commodity
  , _pricePrice :: D.Decimal
  , _priceTargetCommodity :: Commodity
  } deriving (Eq, Show, Ord)

data Transaction = Transaction
  { _transactionFlag :: Flag
  , _transactionDescription :: Text
  , _transactionTags :: [Tag]
  , _transactionPostings :: Accounts
  } deriving (Eq, Show, Ord)

data Balance = Balance
  { _balanceAccount :: Account
  , _balanceAmount :: Amount
  , _balanceCommodity :: Commodity
  } deriving (Eq, Show, Ord)

newtype Close = Close
  { _closeAccount :: Account
  } deriving (Eq, Show, Ord)

data Flag
  = Complete
  | Incomplete
  deriving (Eq, Ord, Show)

newtype Tag =
  Tag Text
  deriving (Eq, Ord, Show)

data Include = Include
  { _includePosition :: P.SourcePos
  , _includeFilePath :: FilePath
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

data Filter
  = NoFilter
  | StrictFilter String
  | Filter String
  | PeriodFilter Date
                 Date
  deriving (Eq, Show)



-- Restrictions
type Restrictions = M.Map Account Restriction

data Restriction
  = NoRestriction
  | RestrictedTo [Commodity]
  deriving (Eq, Ord, Show)

instance Semigroup Restriction where
  RestrictedTo x <> RestrictedTo y = RestrictedTo (x `L.union` y)
  _              <> _              = NoRestriction

instance Monoid Restriction where
  mempty = RestrictedTo []
  RestrictedTo x `mappend` RestrictedTo y = RestrictedTo (x `L.union` y)
  _              `mappend` _              = NoRestriction

isCompatible :: Restriction -> Commodity -> Bool
isCompatible NoRestriction     = const True
isCompatible (RestrictedTo cs) = (`elem` cs)

makePrisms ''Command
makeFields ''Transaction
makeFields ''Open
makeFields ''Close
makeFields ''Price
makeFields ''Balance
makeFields ''Account
makeFields ''Position

balance :: Account -> Commodity -> Accounts -> Amount
balance accountName commodityName =
  M.findWithDefaultM commodityName . fold . M.filterKeys
    (\pos -> pos ^. account == accountName && pos ^. commodity == commodityName)


filter :: Filter -> Ledger -> Ledger
filter (StrictFilter regex) =
--  fmap (fmap (filterPostings regex)) . filter (Filter regex)
  (mapped . mapped . _CmdTransaction . postings %~ M.filterKeys
      (matchAccount regex)
    )
    . filter (Filter regex)
filter (Filter regex) =
  M.filter (not . null) . fmap (L.filter (matchCommand regex))
filter (PeriodFilter dateFrom dateTo) =
  M.filterWithKey (const . between dateFrom dateTo)
filter NoFilter = id

filterPostings :: String -> Command -> Command
filterPostings r = _CmdTransaction . postings %~ M.filterKeys (matchAccount r)

matchCommand :: String -> Command -> Bool
matchCommand regex (CmdTransaction t) =
  anyOf (postings . ifolded . asIndex) (matchAccount regex) t
matchCommand _ (CmdBalance _) = False
matchCommand _ (CmdPrice   _) = False
matchCommand _ _              = True



matchAccount :: String -> Position -> Bool
matchAccount regex = match regex . _positionAccount

match :: Show s => String -> s -> Bool
match regex = (=~ regex) . show


summarize :: Maybe Int -> Accounts -> Accounts
summarize (Just d) = M.mapKeysM $ account %~ shorten d
summarize Nothing  = id

shorten :: Int -> Account -> Account
shorten d = segments %~ take d

eraseLots :: Bool -> Accounts -> Accounts
eraseLots False = M.mapKeysM $ lot .~ Nothing
eraseLots True  = id
