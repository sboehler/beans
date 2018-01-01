module Parser.AST where

import Control.Exception (Exception)
import Control.Lens (makeLenses, makePrisms)
import Data.Decimal (Decimal)
import Data.Text.Lazy (Text, intercalate)
import Data.Text.Prettyprint.Doc
import Data.Time.Calendar (Day)
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
  | Prc Price
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

prettyDay :: Day -> Doc a
prettyDay d = pretty $ show d

data Transaction = Transaction
  { _date :: Day
  , _flag :: Flag
  , _description :: Text
  , _tags :: [Tag]
  , _postings :: [Posting]
  } deriving (Eq, Show)

instance Pretty Transaction where
  pretty Transaction {..} =
    prettyDay _date <+>
    pretty _flag <+>
    dquotes (pretty _description) <+>
    cat (map pretty _tags) <> line <> (indent 2 . vcat) (map pretty _postings)

data Balance = Balance
  { _date :: Day
  , _account :: AccountName
  , _amount :: Decimal
  , _commodity :: CommodityName
  } deriving (Eq, Show)

instance Pretty Balance where
  pretty Balance {..} =
    prettyDay _date <+>
    "balance" <+> pretty _account <+> prettyDec _amount <+> pretty _commodity

data Open = Open
  { _date :: Day
  , _account :: AccountName
  , _commodities :: [CommodityName]
  } deriving (Show, Eq)

instance Pretty Open where
  pretty Open {..} =
    prettyDay _date <+>
    "open" <+> pretty _account <+> sep (map pretty _commodities)

data Close = Close
  { _date :: Day
  , _account :: AccountName
  } deriving (Show, Eq)

instance Pretty Close where
  pretty Close {..} = prettyDay _date <+> "close" <+> pretty _account

data Price = Price
  { _date :: Day
  , _originCommodity :: CommodityName
  , _amount :: Decimal
  , _commodity :: CommodityName
  } deriving (Show, Eq)

instance Pretty Price where
  pretty Price {..} =
    prettyDay _date <+>
    "price" <+>
    pretty _originCommodity <+> prettyDec _amount <+> pretty _commodity

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

prettyDec :: Decimal -> Doc a
prettyDec = pretty . show

data Posting
  = WildcardPosting { _postingAccountName :: AccountName }
  | CompletePosting { _postingAccountName :: AccountName
                    , _amount :: Decimal
                    , _postingCommodityName :: CommodityName
                    , _postingCost :: [PostingCost]
                    , _postingPrice :: Maybe PostingPrice }
  deriving (Show, Eq)

instance Pretty Posting where
  pretty CompletePosting {..} =
    pretty _postingAccountName <+>
    prettyDec _amount <+>
    pretty _postingCommodityName <+>
    prettyCost _postingCost <+> pretty _postingPrice
  pretty (WildcardPosting n) = pretty n

prettyCost :: [PostingCost] -> Doc a
prettyCost [] = mempty
prettyCost c = encloseSep "{" "}" "," (map pretty c)

newtype AccountName =
  AccountName [Text]
  deriving (Show, Eq)

instance Pretty AccountName where
  pretty (AccountName n) = pretty $ ":" `intercalate` n

newtype CommodityName =
  CommodityName Text
  deriving (Show, Eq, Ord)

instance Pretty CommodityName where
  pretty (CommodityName n) = pretty n

data PostingCost
  = PostingCostAmount { _postingCostAmount :: Decimal
                      , _postingCostCommmodity :: CommodityName }
  | PostingCostDate Day
  | PostingCostLabel Text
  deriving (Show, Eq)

instance Pretty PostingCost where
  pretty (PostingCostAmount a c) = prettyDec a <+> pretty c
  pretty (PostingCostDate date) = pretty $ show date
  pretty (PostingCostLabel label) = pretty label

data PostingPrice
  = UnitPrice { _unitPriceAmount :: Decimal
              , _unitPriceCommodity :: CommodityName }
  | TotalPrice { _totalPriceAmount :: Decimal
               , _totalPriceCommodity :: CommodityName }
  deriving (Show, Eq)

instance Pretty PostingPrice where
  pretty (UnitPrice a c) = "@" <+> prettyDec a <+> pretty c
  pretty (TotalPrice a c) = "@@" <+> prettyDec a <+> pretty c

makeLenses ''Transaction

makeLenses ''Tag

makeLenses ''AccountName

makeLenses ''CommodityName

makeLenses ''Posting

makePrisms ''Directive
