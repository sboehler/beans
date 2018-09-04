{-# LANGUAGE GADTs #-}

module Beans.DSL.AST where

import           Beans.Data.Accounts (AccountName (..), Amount)
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import           Data.Time.Calendar  (Day)

-- This is what the language looks like:
--
-- description = /^Postfinance/ && amount == 40 -> Accounts:Konti:Postfinance
-- True -> Accounts:Expenses:Sonstige


type Rules = [Rule]

data Rule =
  Rule (E Bool)
       AccountName
  deriving (Show)


data E a where
  EVarAmount :: E Amount
  EVarDescription :: E Text
  EVarBookingDate :: E Day
  EVarValueDate :: E Day
  EDate :: Day -> E Day
  EAmount :: Amount -> E Amount
  EText :: Text -> E Text
  EBool :: Bool -> E Bool
  EAnd :: E Bool -> E Bool -> E Bool
  EOr :: E Bool -> E Bool -> E Bool
  ENot :: E Bool -> E Bool
  EPlus :: Num a => E a -> E a -> E a
  EMinus :: Num a => E a -> E a -> E a
  ELT :: (Show a, Ord a) => E a -> E a -> E Bool
  ELE :: (Show a, Ord a) => E a -> E a -> E Bool
  EEQ :: (Show a, Eq a) => E a -> E a -> E Bool
  EGE :: (Show a, Ord a) => E a -> E a -> E Bool
  EGT :: (Show a, Ord a) => E a -> E a -> E Bool
  ENE :: (Show a, Ord a) => E a -> E a -> E Bool
  EMatch :: E Text -> E Text -> E Bool

instance Show a => Show (E a) where
  show (EBool a)       = show a
  show (EText a)       = show a
  show (EDate a)       = show a
  show (EAmount a)     = show a
  show EVarAmount      = "amount"
  show EVarDescription = "description"
  show EVarBookingDate = "bookingDate"
  show EVarValueDate   = "valueDate"
  show (EAnd a b)      = "(" <> show a <> " && " <> show b <> ")"
  show (EOr a b)       = "(" <> show a <> " || " <> show b <> ")"
  show (ENot a)        = "!" <> show a
  show (EPlus x y)     = "(" <> show x <> " + " <> show y <> ")"
  show (EMinus x y)    = "(" <> show x <> " - " <> show y <> ")"
  show (ELT a b)       = "(" <> show a <> " < " <> show b <> ")"
  show (ELE a b)       = "(" <> show a <> " <= " <> show b <> ")"
  show (EEQ a b)       = "(" <> show a <> " == " <> show b <> ")"
  show (EGE a b)       = "(" <> show a <> " >= " <> show b <> ")"
  show (EGT a b)       = "(" <> show a <> " > " <> show b <> ")"
  show (ENE a b)       = "(" <> show a <> " <> " <> show b <> ")"
  show (EMatch a b)    = "(" <> show a <> " =~ " <> show b <> ")"
