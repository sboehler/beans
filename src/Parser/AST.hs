module Parser.AST
  ( AccountName(..)
  , CommodityName(..)
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
            , _amount :: Decimal
            , _commodityName :: CommodityName }
  | Transaction { _flag :: Flag
                , _description :: Text
                , _tags :: [Tag]
                , _postings :: [Posting] }
  | Price { _commodity :: CommodityName
          , _amount :: Decimal
          , _priceCommodity :: CommodityName }
  deriving (Eq, Show)

instance Pretty DatedDirective where
  pretty (AccountOpen account commodities) =
    "open" <+> pretty account <+> sep (map pretty commodities)
  pretty (AccountClose account) = "close" <+> pretty account
  pretty (Balance account amount commodity) =
    "balance" <+> pretty account <+> prettyDec amount <+> pretty commodity
  pretty (Price commodity amount priceCommodity) =
    "price" <+> pretty commodity <+> prettyDec amount <+> pretty priceCommodity
  pretty (Transaction flag description tags postings) =
    pretty flag <+>
    dquotes (pretty description) <+>
    cat (map pretty tags) <> line <> (indent 2 . vcat) (map pretty postings)

data ConfigDirective
  = Include FilePath
  | Option Text
           Text
  deriving (Show, Eq)

instance Pretty ConfigDirective where
  pretty (Include filePath) = "include" <+> pretty filePath
  pretty (Option d t) = "option" <+> pretty d <+> pretty t

data Directive
  = Config ConfigDirective
  | Dated { _date :: Day
          , _directive :: DatedDirective }
  deriving (Show, Eq)

instance Pretty Directive where
  pretty (Dated date directive) = pretty (show date) <+> pretty directive
  pretty (Config directive) = pretty directive

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
