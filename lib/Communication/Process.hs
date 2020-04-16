{-# LANGUAGE FlexibleInstances #-}

module Communication.Process
  ( MonadProcess (..)
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))

import           System.IO (Handle)
import qualified System.Process as Process

class MonadProcess m where
  -- | Create a pipe for interprocess communication.
  createPipe :: m (Handle, Handle) -- ^ Read side, write side

instance MonadProcess IO where
  createPipe = Process.createPipe

-- | Automatically lifts 'MonadProcess' through monad transformers
instance {-# OVERLAPPABLE #-} (MonadTrans t, MonadProcess m, Monad m) => MonadProcess (t m) where
  createPipe = lift createPipe
