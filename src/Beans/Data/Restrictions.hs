module Beans.Data.Restrictions
  ( Restrictions
  , Restriction(..)
  , isCompatible
  ) where

import           Beans.Data.Accounts (AccountName, CommodityName)
import qualified Beans.Data.Map      as M
import           Data.List           (union)

type Restrictions = M.Map AccountName Restriction

data Restriction
  = NoRestriction
  | RestrictedTo [CommodityName]
  deriving (Eq, Ord, Show)

instance Monoid Restriction where
  mempty = RestrictedTo []
  RestrictedTo x `mappend` RestrictedTo y = RestrictedTo (x `union` y)
  _ `mappend` _ = NoRestriction

isCompatible :: Restriction -> CommodityName -> Bool
isCompatible r c = case r of
  NoRestriction     -> True
  (RestrictedTo cs) -> c `elem` cs
