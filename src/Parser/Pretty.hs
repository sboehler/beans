{-# OPTIONS_GHC -fno-warn-orphans #-}

module Parser.Pretty where

import Data.Text.Prettyprint.Doc ( Pretty , (<+>) , (<>) , cat , dquotes , encloseSep , indent , line , pretty , sep , vcat)
import Data.Scientific (Scientific)
import Data.Time.Calendar (Day)

import Parser.AST

instance Pretty Scientific where
  pretty = pretty . show

instance Pretty AccountName where
  pretty = pretty . show

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

instance Pretty Lot where
  pretty Lot { _label, _price, _targetCommodity, _date } =
    encloseSep "{" "}" "," $
    [pretty _price, pretty _targetCommodity, pretty _date] ++
    case _label of
      Nothing -> []
      _ -> [pretty _label]

instance Pretty Posting where
  pretty Posting {_account, _amount, _commodity, _lot} =
    pretty _account <+>
    (pretty . show) _amount <+> pretty _commodity <+> pretty _lot
  pretty (Wildcard a) = pretty a

instance Pretty (Directive a) where
  pretty (Opn x _) = pretty x
  pretty (Cls x _) = pretty x
  pretty (Bal x _) = pretty x
  pretty (Trn x _) = pretty x
  pretty (Prc x _) = pretty x
  pretty (Opt x _) = pretty x
  pretty (Inc x _) = pretty x

instance Pretty Balance where
  pretty Balance { _date, _account, _amount, _commodity} =
    pretty _date <+> "balance" <+> pretty _account <+> pretty _amount <+> pretty _commodity

instance Pretty Open where
  pretty Open {..} =
    pretty _date <+>
    "open" <+> pretty _accountName <+> sep (map pretty _commodities)

instance Pretty Close where
  pretty Close {..} = pretty _date <+> "close" <+> pretty _account

instance Pretty Price where
  pretty Price {..} = pretty _date <+> "price" <+> pretty _commodity <+> pretty _price <+> pretty _targetCommodity

instance Pretty Include where
  pretty (Include filePath) = "include" <+> pretty filePath

instance Pretty Option where
  pretty (Option d t) = "option" <+> pretty d <+> pretty t
