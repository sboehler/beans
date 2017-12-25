module Parser.AST
  ( AccountName(..)
  , Amount(..)
  , CommodityName(..)
  , PostingAmount(..)
  , ConfigDirective(..)
  , Posting(..)
  , Directive(..)
  , DatedDirective(..)
  , PostingCost(..)
  , PostingPrice(..)
  , Tag(..)
  , Flag(..)
  ) where

import Data.Decimal (Decimal)
import Data.Text.Lazy (Text, intercalate)
import Data.Text.Prettyprint.Doc
import Data.Time.Calendar (Day)

-- Type to wrap the AST of a file
data DatedDirective
  = AccountOpen { _accountName :: AccountName
                , _commodities :: [CommodityName] }
  | AccountClose { _accountName :: AccountName }
  | Balance { _accountName :: AccountName
            , _amount :: Amount }
  | Transaction { _flag :: Flag
                , _description :: Text
                , _tags :: [Tag]
                , _postings :: [Posting] }
  | Price { _commodity :: CommodityName
          , _amount :: Amount }
  deriving (Eq, Show)

instance Pretty DatedDirective where
  pretty (AccountOpen account commodities) =
    "open" <+> pretty account <+> sep (map pretty commodities)
  pretty (AccountClose account) = "close" <+> pretty account
  pretty (Balance account amount) =
    "balance" <+> pretty account <+> pretty amount
  pretty (Price commodity amount) =
    "price" <+> pretty commodity <+> pretty amount
  pretty (Transaction flag description tags postings) =
    pretty flag <+>
    dquotes (pretty description) <+>
    cat (map pretty tags) <> line <> (indent 2 . vcat) (map pretty postings)

data ConfigDirective
  = Include FilePath
  | Option Text
           Text
  deriving (Show, Eq)

data Directive
  = Config ConfigDirective
  | Dated { _date :: Day
          , _directive :: DatedDirective }
  deriving (Show, Eq)

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

data Amount =
  Amount Decimal
         CommodityName
  deriving (Eq, Show)

instance Pretty Amount where
  pretty (Amount a c) = pretty (show a) <+> pretty c

data Posting = Posting
  { _accountName :: AccountName
  , _postingAmount :: Maybe PostingAmount
  } deriving (Show, Eq)

instance Pretty Posting where
  pretty (Posting n a) = pretty n <+> pretty a

newtype AccountName =
  AccountName [Text]
  deriving (Show, Eq)

instance Pretty AccountName where
  pretty (AccountName n) = pretty $ intercalate ":" n

data PostingAmount = PostingAmount
  { _amount :: Amount
  , _cost :: [PostingCost]
  , _price :: Maybe PostingPrice
  } deriving (Show, Eq)

instance Pretty PostingAmount where
  pretty (PostingAmount amount cost price) =
    pretty amount <+> pretty cost <+> pretty price

newtype CommodityName =
  CommodityName Text
  deriving (Show, Eq, Ord)

instance Pretty CommodityName where
  pretty (CommodityName n) = pretty n

data PostingCost
  = PostingCostAmount Amount
  | PostingCostDate Day
  | PostingCostLabel Text
  deriving (Show, Eq)

instance Pretty PostingCost where
  pretty (PostingCostAmount a) = pretty a
  pretty (PostingCostDate date) = pretty $ show date
  pretty (PostingCostLabel label) = pretty label

data PostingPrice
  = UnitPrice { _amount :: Amount }
  | TotalPrice { _amount :: Amount }
  deriving (Show, Eq)

instance Pretty PostingPrice where
  pretty (UnitPrice a) = "@" <+> pretty a
  pretty (TotalPrice a) = "@@" <+> pretty a
