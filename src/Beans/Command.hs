module Beans.Command
  ( Command (..),
    Directive (..),
    date,
  )
where

import Beans.Assertion (Assertion (Assertion))
import Beans.Close (Close (Close))
import Beans.Date (Date)
import Beans.Include (Include)
import Beans.Open (Open (Open))
import Beans.Option (Option)
import Beans.Price (Price (Price))
import Beans.Transaction (Transaction (Transaction))
import Data.Text.Prettyprint.Doc (Pretty (pretty))

data Command
  = CmdOpen Open
  | CmdPrice Price
  | CmdTransaction Transaction
  | CmdAssertion Assertion
  | CmdClose Close
  deriving (Eq, Show, Ord)

instance Pretty Command where
  pretty (CmdTransaction t) = pretty t
  pretty (CmdPrice t) = pretty t
  pretty (CmdOpen t) = pretty t
  pretty (CmdClose t) = pretty t
  pretty (CmdAssertion t) = pretty t

date :: Command -> Date
date (CmdTransaction (Transaction d _ _ _)) = d
date (CmdPrice (Price d _ _ _)) = d
date (CmdOpen (Open d _)) = d
date (CmdClose (Close d _)) = d
date (CmdAssertion (Assertion d _ _ _)) = d

data Directive
  = CmdDirective (Int, Int) Command
  | OptionDirective Option
  | IncludeDirective Include
  deriving (Eq, Show)

instance Pretty Directive where
  pretty (CmdDirective _ d) = pretty d
  pretty (OptionDirective o) = pretty o
  pretty (IncludeDirective i) = pretty i
