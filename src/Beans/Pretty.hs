{-# OPTIONS_GHC -fno-warn-orphans #-}

module Beans.Pretty
  ( prettyPrint
  , pretty
  , prettyPrintLedger
  )
where

import           Beans.Model
import qualified Beans.Data.Map                as M
import qualified Data.Text                     as T
import qualified Data.Decimal                  as D
import           Data.Text.Prettyprint.Doc

instance Pretty Amount where
  pretty = pretty . format

instance Pretty D.Decimal where
  pretty = pretty . show

instance Pretty Account where
  pretty = pretty . show

instance Pretty Commodity where
  pretty (Commodity c) = pretty c

instance Pretty Date where
  pretty = pretty . show


prettyAccounts :: Accounts -> [Doc a]
prettyAccounts = concatMap p . M.toList
 where
  p (Position a _ l, v) = fmap g (M.toList v)
    where g (c, s) = pretty a <+> pretty s <+> pretty c <+> pretty l

instance Pretty Flag where
  pretty Complete   = "*"
  pretty Incomplete = "!"

instance Pretty Tag where
  pretty (Tag t) = pretty t

instance Pretty Lot where
  pretty Lot {_lotLabel, _lotPrice, _lotTargetCommodity, _lotDate} =
    encloseSep "{" "}" "," $
    [pretty _lotPrice <+> pretty _lotTargetCommodity, pretty _lotDate] ++
    case _lotLabel of
      Nothing -> []
      Just l  -> [pretty l]

instance Pretty Directive where
  pretty (DatedCommandDirective d) = pretty d
  pretty (OptionDirective o)       = pretty o
  pretty (IncludeDirective i)      = pretty i

instance Pretty a => Pretty (Dated a) where
  pretty (Dated day x) = pretty day <+> pretty x

instance Pretty Command where
  pretty (CmdTransaction t) = pretty t
  pretty (CmdPrice t) = pretty t
  pretty (CmdOpen t) = pretty t
  pretty (CmdClose t) = pretty t
  pretty (CmdBalance t) = pretty t

instance Pretty Transaction where
  pretty Transaction {..} =
    pretty _transactionFlag <+>
    dquotes (pretty $ quote _transactionDescription) <+>
    cat (pretty <$> _transactionTags) <> line <> (indent 2 . vcat) (prettyAccounts _transactionPostings) <> hardline

quote :: T.Text -> T.Text
quote = T.replace "\"" "\\\""

instance Pretty Balance where
  pretty Balance {_balanceAccount, _balanceAmount, _balanceCommodity} =
    "balance" <+> pretty _balanceAccount <+> pretty _balanceAmount <+> pretty _balanceCommodity
instance Pretty Open where
  pretty Open {_openAccount, _openRestriction} =
    "open" <+> pretty _openAccount <+> pretty _openRestriction
instance Pretty Close where
  pretty Close {_closeAccount} = "close" <+> pretty _closeAccount
instance Pretty Price where
  pretty Price {_priceCommodity, _pricePrice, _priceTargetCommodity} =
    "price" <+> pretty _priceCommodity <+> pretty _pricePrice <+> pretty _priceTargetCommodity

instance Pretty Restriction where
  pretty NoRestriction    = mempty
  pretty (RestrictedTo c) = hsep (map pretty c)


instance Pretty Include where
  pretty (Include _ filePath) = "include" <+> pretty filePath

instance Pretty Option where
  pretty (Option _ d t) = "option" <+> pretty d <+> pretty t

prettyPrint :: [Directive] -> IO ()
prettyPrint = print . vsep . map ((<> hardline) . pretty)

prettyPrintLedger :: Ledger -> IO ()
prettyPrintLedger = print . vsep . fmap ((<> hardline) . pretty) . M.toList
