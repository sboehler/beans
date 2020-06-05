module Beans.LedgerStep
  ( LedgerStep (LS),
    new,
    add,
    insert,
    valuate,
  )
where

import Beans.Assertion (Assertion)
import Beans.Close (Close)
import Beans.Command (Command (..))
import Beans.Date (Date)
import Beans.Open (Open)
import Beans.Price (Price)
import Beans.Prices (NormalizedPrices)
import Beans.Transaction (Transaction)
import qualified Beans.Transaction as Transaction
import Control.Monad.Catch (MonadThrow)
import Data.Text.Prettyprint.Doc (Pretty (pretty))
import Data.Traversable (for)

data LedgerStep = LS Date [Open] [Price] [Transaction] [Assertion] [Close]
  deriving (Eq, Show)

instance Pretty LedgerStep where
  pretty _s = undefined

new :: Date -> LedgerStep
new d = LS d [] [] [] [] []

insert :: Command -> LedgerStep -> LedgerStep
insert (CmdOpen o') (LS d o p t b c) = LS d (o' : o) p t b c
insert (CmdPrice p') (LS d o p t b c) = LS d o (p' : p) t b c
insert (CmdTransaction t') (LS d o p t b c) = LS d o p (t' : t) b c
insert (CmdAssertion b') (LS d o p t b c) = LS d o p t (b' : b) c
insert (CmdClose c') (LS d o p t b c) = LS d o p t b (c' : c)

valuate :: MonadThrow m => LedgerStep -> NormalizedPrices -> m LedgerStep
valuate (LS d o p t b c) np = do
  t' <- for t $ Transaction.valuate np
  pure $ LS d o p t' b c

add :: [Transaction] -> LedgerStep -> LedgerStep
add t' (LS d o p t b c) = LS d o p (t ++ t') b c
