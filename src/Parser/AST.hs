module Parser.AST where

import Control.Exception (Exception)
import Control.Lens (makeLenses, makePrisms)
import Data.Accounts (AccountName(..))
import Data.Amount (Amount)
import Data.Commodity (CommodityName(..))
import Data.Date (Date)
import Data.Decimal (Decimal)
import Data.Price (Price(..))
import Data.Text.Lazy (Text)
import Data.Text.Prettyprint.Doc
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

data Transaction = Transaction
  { _date :: Date
  , _flag :: Flag
  , _description :: Text
  , _tags :: [Tag]
  , _postings :: [Posting]
  } deriving (Eq, Show)

instance Pretty Transaction where
  pretty Transaction {..} =
    pretty _date <+>
    pretty _flag <+>
    dquotes (pretty _description) <+>
    cat (map pretty _tags) <> line <> (indent 2 . vcat) (map pretty _postings)

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

data Flag
  = Complete
  | Incomplete
  deriving (Eq, Show)

instance Pretty Flag where
  pretty Complete = "*"
  pretty Incomplete = "!"

newtype Tag =
  Tag Text
  deriving (Show, Eq)

instance Pretty Tag where
  pretty (Tag t) = pretty t

data PostingDirective
  = WildcardPosting AccountName
  | CompletePosting Posting
  deriving (Show, Eq)

instance Pretty PostingDirective where
  pretty (CompletePosting p) = pretty p
  pretty (WildcardPosting n) = pretty n

data Posting = Posting
  { _postingAccountName :: AccountName
  , _amount :: Amount Decimal
  , _postingCost :: [PostingCost]
  , _postingPrice :: Maybe PostingPrice
  } deriving (Show, Eq)

instance Pretty Posting where
  pretty Posting {..} =
    pretty _postingAccountName <+>
    pretty _amount <+> prettyCost _postingCost <+> pretty _postingPrice

prettyCost :: [PostingCost] -> Doc a
prettyCost [] = mempty
prettyCost c = encloseSep "{" "}" "," (map pretty c)

data PostingCost
  = PostingCostAmount { _price :: Price Decimal }
  | PostingCostDate Date
  | PostingCostLabel Text
  deriving (Show, Eq)

instance Pretty PostingCost where
  pretty (PostingCostAmount p) = pretty p
  pretty (PostingCostDate date) = pretty $ show date
  pretty (PostingCostLabel label) = pretty label

data PostingPrice
  = UnitPrice { _price :: Price Decimal }
  | TotalAmount { _amount :: Amount Decimal }
  deriving (Show, Eq)

instance Pretty PostingPrice where
  pretty (UnitPrice p) = "@" <+> pretty p
  pretty (TotalAmount a) = "@@" <+> pretty a

makeLenses ''Transaction

makeLenses ''Tag

makeLenses ''AccountName

makeLenses ''CommodityName

makeLenses ''Posting

makePrisms ''Directive

makePrisms ''Posting

makePrisms ''PostingPrice
