{-# LANGUAGE GADTs #-}

module Beans.DSL.Interpreter
  ( evalRules
  ) where

import           Beans.Data.Accounts  (AccountName (..))
import           Beans.DSL.AST        (E (..), Rule (..), Rules)
import           Beans.Import.Common  (Entry (..))
import           Control.Monad        (msum)
import           Control.Monad.Reader (Reader, asks)
import           Data.Bool            (bool)
import           Data.Maybe           (fromMaybe)


evalRules :: AccountName -> Rules -> Reader Entry AccountName
evalRules c rs = fromMaybe c . msum <$> sequence (evalRule <$> rs)

evalRule :: Rule -> Reader Entry (Maybe AccountName)
evalRule (Rule e c) = bool Nothing (Just c) <$> evalE e

evalE :: E a -> Reader Entry a
evalE (EBool   a)     = return a
evalE (EText   a)     = return a
evalE (EDate   a)     = return a
evalE (EAmount a)     = return a
evalE EVarAmount      = asks _amount
evalE EVarDescription = asks _description
evalE EVarBookingDate = asks _bookingDate
evalE EVarValueDate   = asks _valueDate
evalE (EAnd a b  )    = (&&) <$> evalE a <*> evalE b
evalE (EOr  a b  )    = (||) <$> evalE a <*> evalE b
evalE (ENot a    )    = not <$> evalE a
evalE (EPlus  x y)    = (+) <$> evalE x <*> evalE y
evalE (EMinus x y)    = (-) <$> evalE x <*> evalE y
evalE (ELT    a b)    = (<) <$> evalE a <*> evalE b
evalE (ELE    a b)    = (<=) <$> evalE a <*> evalE b
evalE (EEQ    a b)    = (==) <$> evalE a <*> evalE b
evalE (EGE    a b)    = (>=) <$> evalE a <*> evalE b
evalE (EGT    a b)    = (>) <$> evalE a <*> evalE b
evalE (ENE    a b)    = (/=) <$> evalE a <*> evalE b
evalE (EMatch _ _)    = return True
