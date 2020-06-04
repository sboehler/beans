module Beans.Ledger
  ( Ledger (Ledger),
    new,
    fromDirectives,
    minDate,
    maxDate,
    merge,
    splitAtDates,
    transform,
  )
where

import Beans.Command (Directive (CmdDirective))
import qualified Beans.Command as Command
import Beans.Date (Date)
import Beans.LedgerStep (LedgerStep (LS))
import qualified Beans.LedgerStep as LedgerStep
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict.Extended as Map
import Data.Map.Strict.Extended (Map)
import Data.Maybe (fromMaybe)
import Data.Text.Prettyprint.Doc ((<+>), Pretty (pretty), hardline, vsep)
import Data.Traversable (for)
import Prelude hiding (filter)

-- Ledger

data LedgerException = EmptyLedgerException deriving (Eq, Show)

instance Exception LedgerException

newtype Ledger
  = Ledger
      { ledger :: Map Date LedgerStep
      }
  deriving (Show, Eq)

instance Pretty Ledger where
  pretty = vsep . Map.elems . fmap ((<+> hardline) . pretty) . ledger

new :: Ledger
new = Ledger mempty

fromDirectives :: [Directive] -> Ledger
fromDirectives directives = Ledger $ foldr add Map.empty directives
  where
    add (CmdDirective _ cmd) = Map.alter (Just . upsert (Command.date cmd) cmd) (Command.date cmd)
    add _ = id
    upsert d cmd step = LedgerStep.insert cmd (fromMaybe (LedgerStep.new d) step)

minDate :: MonadThrow m => Ledger -> m Date
minDate (Ledger m) = case m' of
  Just (LS d _ _ _ _ _) -> pure d
  Nothing -> throwM EmptyLedgerException
  where
    m' = Foldable.find (\(LS _ _ _ t _ _) -> not $ null t) m

maxDate :: MonadThrow m => Ledger -> m Date
maxDate (Ledger m) = case fst <$> Map.lookupMax m of
  Just d -> pure d
  Nothing -> throwM EmptyLedgerException

splitAtDates :: Ledger -> [Date] -> [Ledger]
splitAtDates _ [] = []
splitAtDates (Ledger l) (d : ds) = Ledger l1 : splitAtDates (Ledger l2) ds
  where
    (l1, l2) = Map.spanAntitone (<= d) l

merge :: Ledger -> Ledger -> Ledger
merge (Ledger l1) (Ledger l2) = Ledger $ Map.union l1 l2

transform :: Monad m => (LedgerStep -> m LedgerStep) -> Ledger -> m Ledger
transform f (Ledger l) = Ledger <$> for l f
