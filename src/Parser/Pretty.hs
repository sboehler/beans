{-# OPTIONS_GHC -fno-warn-orphans #-}

module Parser.Pretty where

import qualified Data.Map.Lazy as M
import Data.Text.Prettyprint.Doc
       (Pretty, (<+>), (<>), align, cat, dquotes, encloseSep, indent,
        line, nest, pretty, sep, vcat, vsep)
import Data.Time.Calendar (Day)

import Data.Account (Account(..))
import Data.AccountName (AccountName(..))
import Data.Accounts (Accounts(..))
import Data.Amount (Amount(..))
import Data.Commodity (CommodityName(..))
import Data.Holdings (Holdings(..))
import Data.Lot (Lot(..))
import Data.Posting (Posting(..), PostingPrice(..))
import Data.Transaction (Flag(..), Tag(..), Transaction(..))
import Parser.AST
       (Balance(..), Close(..), Directive(..), Include(..), Open(..),
        Option(..), PostingDirective(..), Price(..))

instance Pretty Account where
  pretty (Account a h) = pretty h <+> line <+> nest 2 (pretty a)

instance (Pretty a) => Pretty (Accounts a) where
  pretty (Accounts m) = vsep (fmap f (M.toList m))
    where
      f (name, account) = pretty name <+> pretty account

instance Pretty AccountName where
  pretty = pretty . show

instance Pretty Amount where
  pretty Amount {..} = pretty (show _amount) <+> pretty _commodity

instance Pretty CommodityName where
  pretty = pretty . _unCommodityName

instance Pretty Day where
  pretty = pretty . show

instance Pretty Transaction where
  pretty Transaction {..} =
    pretty _date <+>
    pretty _flag <+>
    dquotes (pretty _description) <+>
    cat (map pretty _tags) <> line <> (indent 2 . vcat) (map pretty _postings)

instance Pretty Flag where
  pretty Complete = "*"
  pretty Incomplete = "!"

instance Pretty Tag where
  pretty (Tag t) = pretty t

instance Pretty Holdings where
  pretty (Holdings h) = align $ vsep (map f (M.toList h))
    where
      f (c, a) = (pretty . show) a <+> pretty c

instance Pretty Lot where
  pretty Lot {..} =
    encloseSep "{" "}" "," $
    [pretty _cost, pretty _date] ++
    case _label of
      Nothing -> []
      _ -> [pretty _label]

instance Pretty Posting where
  pretty Posting {..} =
    pretty _accountName <+>
    (pretty . show) _amount <+>
    pretty _commodity <+> pretty _price <+> pretty _lot

instance Pretty PostingPrice where
  pretty (UnitPrice p) = "@" <+> pretty p
  pretty (TotalPrice a) = "@@" <+> pretty a

instance Pretty (Directive a) where
  pretty (Opn x _) = pretty x
  pretty (Cls x _) = pretty x
  pretty (Bal x _) = pretty x
  pretty (Trn x _) = pretty x
  pretty (Prc x _) = pretty x
  pretty (Opt x _) = pretty x
  pretty (Inc x _) = pretty x

instance Pretty Balance where
  pretty Balance {..} =
    pretty _date <+> "balance" <+> pretty _accountName <+> pretty _amount

instance Pretty Open where
  pretty Open {..} =
    pretty _date <+>
    "open" <+> pretty _accountName <+> sep (map pretty _commodities)

instance Pretty Close where
  pretty Close {..} = pretty _date <+> "close" <+> pretty _accountName

instance Pretty Price where
  pretty Price {..} = pretty _date <+> "price" <+> pretty _price

instance Pretty Include where
  pretty (Include filePath) = "include" <+> pretty filePath

instance Pretty Option where
  pretty (Option d t) = "option" <+> pretty d <+> pretty t

instance Pretty PostingDirective where
  pretty (CompletePosting p) = pretty p
  pretty (WildcardPosting n) = pretty n
