module Beans.Assertion
  ( Assertion (Assertion),
    match,
  )
where

import Beans.Account (Account)
import qualified Beans.Account as Account
import Beans.Amount (Amount)
import Beans.Commodity (Commodity)
import qualified Beans.Commodity as Commodity
import Beans.Date (Date)
import Beans.Options (Filter (Filter))
import Data.Text.Prettyprint.Doc ((<+>), Pretty (pretty))

data Assertion
  = Assertion Date Account Amount Commodity
  deriving (Eq, Show, Ord)

instance Pretty Assertion where
  pretty (Assertion d a amt c) =
    pretty d
      <+> "balance"
      <+> pretty a
      <+> pretty amt
      <+> pretty c

match :: Filter -> Assertion -> Bool
match (Filter af cf) (Assertion _ a _ c) = Account.match af a && Commodity.match cf c
