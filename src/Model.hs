module Model
  ( AccountName(..)
  , CommodityName(..)
  , PostingAmount(..)
  , ConfigDirective(..)
  , Posting(..)
  , Directive(..)
  , DatedDirective(..)
  , PostingPrice(..)
  , Tag(..)
  , Cost(..)
  , Flag(..)
  ) where

import Data.Decimal (Decimal)
import Data.Text.Lazy (Text, intercalate, pack)
import Data.Text.Prettyprint.Doc
import Data.Time.Calendar (Day)

-- Type to wrap the AST of a file
data DatedDirective
  = AccountOpen { _accountName :: AccountName
                , _commodities :: [CommodityName] }
  | AccountClose { _accountName :: AccountName }
  | Balance { _accountName :: AccountName
            , _amount :: Decimal
            , _commodity :: CommodityName }
  | Transaction { _flag :: Flag
                , _description :: Text
                , _tags :: [Tag]
                , _postings :: [Posting] }
  | Price { _commodity :: CommodityName
          , _price :: Decimal
          , _priceCommodity :: CommodityName }
  deriving (Eq, Show)

instance Pretty DatedDirective where
  pretty (AccountOpen n c) =
    pretty (pack "open") <+> pretty n <+> sep (map pretty c)
  pretty (AccountClose n) = pretty (pack "close") <+> pretty n
  pretty (Balance n a c) =
    pretty (pack "balance") <+> pretty n <+> pretty (show a) <+> pretty c
  pretty (Price c p pc) =
    pretty (pack "price") <+> pretty c <+> pretty (show p) <+> pretty pc
  pretty (Transaction f d ts ps) =
    pretty f <+>
    dquotes (pretty d) <+>
    cat (map pretty ts) <+> line <> (indent 2 . vcat) (map pretty ps)

data ConfigDirective
  = Include FilePath
  | Option Text
           Text
  deriving (Show, Eq)

-- Type to wrap the AST of a file
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

data Posting = Posting
  { _accountName :: AccountName
  , _amount :: Maybe PostingAmount
  } deriving (Show, Eq)

instance Pretty Posting where
  pretty (Posting n a) = pretty n <+> pretty a

newtype AccountName =
  AccountName [Text]
  deriving (Show, Eq)

instance Pretty AccountName where
  pretty (AccountName n) = pretty $ intercalate ":" n

data PostingAmount = PostingAmount
  { _amount :: Decimal
  , _commodity :: CommodityName
  , _cost :: Maybe Cost
  , _price :: Maybe PostingPrice
  } deriving (Show, Eq)

instance Pretty PostingAmount where
  pretty (PostingAmount a c cost price) =
    pretty (show a) <+> pretty c <+> pretty cost <+> pretty price

newtype CommodityName =
  CommodityName Text
  deriving (Show, Eq)

instance Pretty CommodityName where
  pretty (CommodityName n) = pretty n

data Cost = Cost
  { _amount :: Decimal
  , _commodity :: CommodityName
  , _label :: Maybe Day
  } deriving (Show, Eq)

instance Pretty Cost where
  pretty (Cost a c l) =
    braces $ pretty (show a) <+> pretty c <+> pretty (show l)

data PostingPrice
  = UnitPrice { _amount :: Decimal
              , _commodity :: CommodityName }
  | TotalPrice { _amount :: Decimal
               , _commodity :: CommodityName }
  deriving (Show, Eq)

instance Pretty PostingPrice where
  pretty (UnitPrice a c) = pretty (show a) <+> pretty c
  pretty (TotalPrice a c) = pretty (show a) <+> pretty c
