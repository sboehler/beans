module Parser.AST where

import Control.Exception (Exception)
import Data.Account (AccountName(..))
import Data.Amount (Amount)
import Data.Commodity (CommodityName(..))
import Data.Date (Date)
import Data.Decimal (Decimal)
import Data.Posting (Posting)
import Data.Price (Price(..))
import Data.Text.Lazy (Text)
import Data.Text.Prettyprint.Doc
import Data.Transaction (Transaction)
import Text.Parsec (ParseError)

newtype ParseException =
  ParseException ParseError
  deriving (Show)

instance Exception ParseException

-- Type to wrap the AST of a file
data Directive a
  = Opn Open
        a
  | Cls Close
        a
  | Bal Balance
        a
  | Trn Transaction
        a
  | Prc PriceDirective
        a
  | Opt Option
        a
  | Inc Include
        a
  deriving (Eq, Show, Functor)

instance Pretty (Directive a) where
  pretty (Opn x _) = pretty x
  pretty (Cls x _) = pretty x
  pretty (Bal x _) = pretty x
  pretty (Trn x _) = pretty x
  pretty (Prc x _) = pretty x
  pretty (Opt x _) = pretty x
  pretty (Inc x _) = pretty x

data Balance = Balance
  { _date :: Date
  , _accountName :: AccountName
  , _amount :: Amount Decimal
  } deriving (Eq, Show)

instance Pretty Balance where
  pretty Balance {..} =
    pretty _date <+> "balance" <+> pretty _accountName <+> pretty _amount

data Open = Open
  { _date :: Date
  , _accountName :: AccountName
  , _commodities :: [CommodityName]
  } deriving (Show, Eq)

instance Pretty Open where
  pretty Open {..} =
    pretty _date <+>
    "open" <+> pretty _accountName <+> sep (map pretty _commodities)

data Close = Close
  { _date :: Date
  , _accountName :: AccountName
  } deriving (Show, Eq)

instance Pretty Close where
  pretty Close {..} = pretty _date <+> "close" <+> pretty _accountName

data PriceDirective = PriceDirective
  { _date :: Date
  , _price :: Price Decimal
  } deriving (Show, Eq)

instance Pretty PriceDirective where
  pretty PriceDirective {..} = pretty _date <+> "price" <+> pretty _price

data PostingCost
  = PostingCostAmount (Price Decimal)
  | PostingCostLabel Text
  | PostingCostDate Date

newtype Include = Include
  { _filePath :: FilePath
  } deriving (Show, Eq)

instance Pretty Include where
  pretty (Include filePath) = "include" <+> pretty filePath

data Option =
  Option Text
         Text
  deriving (Show, Eq)

instance Pretty Option where
  pretty (Option d t) = "option" <+> pretty d <+> pretty t

data PostingDirective
  = WildcardPosting AccountName
  | CompletePosting Posting
  deriving (Show, Eq)

instance Pretty PostingDirective where
  pretty (CompletePosting p) = pretty p
  pretty (WildcardPosting n) = pretty n
