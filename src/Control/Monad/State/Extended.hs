module Control.Monad.State.Extended
  ( module Control.Monad.State,
    put',
    modifyM,
  )
where

import Control.Monad.State

put' :: MonadState s m => s -> m s
put' s = put s >> pure s

modifyM :: MonadState s m => (s -> m s) -> m s
modifyM f = get >>= f >>= put'
