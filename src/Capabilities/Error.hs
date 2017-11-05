module Capabilities.Error
  ( module Control.Monad.Freer.Error
  , Err(..)
  , AppError
  ) where

import Control.Monad.Freer.Error

data Err
  = NotFound
  | InternalError
  | Unauthorized
  | BadRequest
  deriving (Eq, Show)

type AppError = Error Err
