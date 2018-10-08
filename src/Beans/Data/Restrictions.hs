module Beans.Data.Restrictions
  ( Restrictions
  , Restriction(..)
  , isCompatible
  ) where

import           Beans.Data.Accounts (Account, Commodity)
import qualified Beans.Data.Map      as M
import           Data.List           (union)

type Restrictions = M.Map Account Restriction

data Restriction
  = NoRestriction
  | RestrictedTo [Commodity]
  deriving (Eq, Ord, Show)

instance Semigroup Restriction where
  RestrictedTo x <> RestrictedTo y = RestrictedTo (x `union` y)
  _ <> _ = NoRestriction

instance Monoid Restriction where
  mempty = RestrictedTo []
  RestrictedTo x `mappend` RestrictedTo y = RestrictedTo (x `union` y)
  _ `mappend` _ = NoRestriction

isCompatible :: Restriction -> Commodity -> Bool
isCompatible r c = case r of
  NoRestriction     -> True
  (RestrictedTo cs) -> c `elem` cs
