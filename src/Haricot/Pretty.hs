{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haricot.Pretty
  ( prettyPrint
  , prettyPrintLedger
  , prettyPrintAccounts
  ) where

import qualified Data.Map.Strict           as M
import           Data.Scientific           (Scientific)
import           Data.Text.Prettyprint.Doc
import           Data.Time.Calendar        (Day)
import           Haricot.Accounts
import           Haricot.AST
import           Haricot.Ledger

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
    pretty _flag <+>
    dquotes (pretty _description) <+>
    cat (map pretty _tags) <> line <> (indent 2 . vcat) (map pretty _postings)

instance Pretty Flag where
  pretty Complete   = "*"
  pretty Incomplete = "!"

instance Pretty Tag where
  pretty (Tag t) = pretty t

instance Pretty Lot where
  pretty Lot {_label, _price, _targetCommodity, _date} =
    encloseSep "{" "}" "," $
    [pretty _price, pretty _targetCommodity, pretty _date] ++
    case _label of
      Nothing -> []
      _       -> [pretty _label]

instance Pretty Posting where
  pretty (CP p) = pretty p
  pretty (WP w) = pretty w

instance Pretty CompletePosting where
  pretty CompletePosting {_account, _amount, _commodity, _lot} =
    pretty _account <+>
    (pretty . show) _amount <+> pretty _commodity <+> pretty _lot

instance Pretty WildcardPosting where
  pretty (WildcardPosting _ a) = pretty a

instance Pretty Directive where
  pretty (Opt x) = pretty x
  pretty (Inc x) = pretty x
  pretty (Bal b) = pretty b
  pretty (Opn o) = pretty o
  pretty (Cls c) = pretty c
  pretty (Trn t) = pretty t
  pretty (Prc p) = pretty p

instance Pretty Balance where
  pretty Balance {_account, _amount, _commodity} =
    "balance" <+> pretty _account <+> pretty _amount <+> pretty _commodity

instance Pretty Open where
  pretty Open {_account, _restriction} =
    "open" <+> pretty _account <+> pretty _restriction

instance Pretty Restriction where
  pretty NoRestriction    = mempty
  pretty (RestrictedTo c) = hsep (map pretty c)

instance Pretty Close where
  pretty Close {_account} = "close" <+> pretty _account

instance Pretty Price where
  pretty Price {_commodity, _price, _targetCommodity} =
    "price" <+> pretty _commodity <+> pretty _price <+> pretty _targetCommodity

instance Pretty Include where
  pretty (Include _ filePath) = "include" <+> pretty filePath

instance Pretty Option where
  pretty (Option _ d t) = "option" <+> pretty d <+> pretty t

prettyPrint :: [Directive] -> IO ()
prettyPrint = print . vsep . map ((<> hardline) . pretty)

prettyPrintLedger :: Ledger -> IO()
prettyPrintLedger = print . vsep . fmap ((<> hardline) . pretty . snd) . M.toList

instance Pretty Timestep where
  pretty Timestep{_date, _openings, _closings, _balances, _transactions} =
    pretty _date <> hardline <>
      vsep (map pretty _openings) <> hardline <>
      vsep (map pretty _closings) <> hardline <>
      vsep (map pretty _transactions)

prettyPrintAccounts :: Accounts -> IO ()
prettyPrintAccounts accounts = print $ vsep $ map p (M.toList accounts)
  where
    p (k,v) = pretty k <+> pretty v


instance Pretty Account where
  pretty Account {_holdings}= (vsep . map p . M.toList) _holdings
    where
      p (k, v) = pretty k


