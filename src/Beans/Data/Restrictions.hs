module Beans.Data.Restrictions where

import Beans.Data.Accounts (AccountName, CommodityName)
import qualified Beans.Data.Map as M
import Data.List (union)

type Restrictions = M.Map AccountName Restriction

data Restriction
  = NoRestriction
  | RestrictedTo [CommodityName]
  deriving (Show)

instance Monoid Restriction where
  mempty = RestrictedTo []
  RestrictedTo x `mappend` RestrictedTo y = RestrictedTo (x `union` y)
  _ `mappend` _ = NoRestriction

isOpen :: AccountName -> Restrictions -> Bool
isOpen = M.member

find :: AccountName -> Restrictions -> Maybe Restriction
find = M.find

split :: AccountName -> Restrictions -> (Restrictions, Restrictions)
split a = M.split (== a)

isCompatible :: Restriction -> CommodityName -> Bool
isCompatible r c = case r of
  NoRestriction     -> True
  (RestrictedTo cs) -> c `elem` cs

add :: AccountName -> Restriction -> Restrictions -> Restrictions
add a r = M.insert a (const r)

isEmpty :: Restrictions -> Bool
isEmpty = M.isEmpty
