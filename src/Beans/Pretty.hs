{-# OPTIONS_GHC -fno-warn-orphans #-}

module Beans.Pretty
  ( prettyPrint
  , prettyPrintLedger
  ) where

import           Beans.Data.Accounts       (AccountName (..), Accounts, Amount,
                                            CommodityName (..), Lot (..))
import           Beans.Data.Directives     (Balance (..), Close (..),
                                            Command (..), DatedCommand (..),
                                            Directive (..), Flag (..),
                                            Include (..), Open (..),
                                            Option (..), Price (..), Tag (..),
                                            Transaction (..))
import qualified Beans.Data.Map            as M
import           Beans.Data.Restrictions   (Restriction (..))
import           Beans.Ledger              (Ledger, Timestep (Timestep), toList)
import           Data.Monoid               (Sum (..))
import           Data.Scientific           (Scientific)
import           Data.Text.Prettyprint.Doc
import           Data.Time.Calendar        (Day)

instance Pretty Amount where
  pretty (Sum a) = pretty $ show a

instance Pretty Scientific where
  pretty = pretty . show

instance Pretty AccountName where
  pretty = pretty . show

instance Pretty CommodityName where
  pretty (CommodityName c) = pretty c

instance Pretty Day where
  pretty = pretty . show

instance Pretty Transaction where
  pretty Transaction {..} =
    pretty tFlag <+>
    dquotes (pretty tDescription) <+>
    cat (pretty <$> tTags) <> line <> (indent 2 . vcat) (prettyAccounts tPostings)

prettyAccounts :: Accounts -> [Doc a]
prettyAccounts  = concatMap p . M.toList
  where
    p ((a, _, l), v) = fmap g (M.toList v)
      where
        g (c, s) = pretty a <+> pretty s <+> pretty c <+> pretty l <+> hardline

instance Pretty Flag where
  pretty Complete   = "*"
  pretty Incomplete = "!"

instance Pretty Tag where
  pretty (Tag t) = pretty t

instance Pretty Lot where
  pretty Lot {lLabel, lPrice, lTargetCommodity, lDate} =
    encloseSep "{" "}" "," $
    [pretty lPrice, pretty lTargetCommodity, pretty lDate] ++
    case lLabel of
      Nothing -> []
      Just l  -> [pretty l]

instance Pretty Directive where
  pretty (DatedCommandDirective (DatedCommand day command)) =
    pretty day <+> pretty command
  pretty (OptionDirective o) = pretty o
  pretty (IncludeDirective i) = pretty i

instance Pretty Command where
  pretty (BalanceCommand b)     = pretty b
  pretty (OpenCommand o)        = pretty o
  pretty (CloseCommand c)       = pretty c
  pretty (TransactionCommand t) = pretty t
  pretty (PriceCommand p)       = pretty p

instance Pretty Balance where
  pretty Balance {bAccount, bAmount, bCommodity} =
    "balance" <+> pretty bAccount <+> pretty bAmount <+> pretty bCommodity

instance Pretty Open where
  pretty Open {oAccount, oRestriction} =
    "open" <+> pretty oAccount <+> pretty oRestriction

instance Pretty Restriction where
  pretty NoRestriction    = mempty
  pretty (RestrictedTo c) = hsep (map pretty c)

instance Pretty Close where
  pretty Close {cAccount} = "close" <+> pretty cAccount

instance Pretty Price where
  pretty Price {pCommodity, pPrice, pTargetCommodity} =
    "price" <+> pretty pCommodity <+> pretty pPrice <+> pretty pTargetCommodity

instance Pretty Include where
  pretty (Include _ filePath) = "include" <+> pretty filePath

instance Pretty Option where
  pretty (Option _ d t) = "option" <+> pretty d <+> pretty t

prettyPrint :: [Directive] -> IO ()
prettyPrint = print . vsep . map ((<> hardline) . pretty)

prettyPrintLedger :: Ledger -> IO ()
prettyPrintLedger = print . vsep . fmap ((<> hardline) . pretty) . toList

instance Pretty Timestep where
  pretty (Timestep day commands) = vsep (map pretty' commands) <> hardline
    where
      pretty' command = pretty day <+> pretty command
