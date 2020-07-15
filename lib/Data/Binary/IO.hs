{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Read and write values of types that implement 'Binary.Binary' from and to 'Handle's
--
-- This module homes the unlifted API variant. For proper documentation check out the equally named
-- functions in "Data.Binary.IO.Lifted"
--
module Data.Binary.IO
  ( -- * Reader
    Lifted.ReaderError (..)

  , Reader (..)
  , newReader
  , newReaderWith

    -- * Writer
  , Writer (..)
  , newWriter
  , newWriterWith

    -- * Pipe
  , newPipe

    -- * Duplex
  , Duplex (..)
  , newDuplex
  , newDuplexWith

    -- * Classes
  , CanGet
  , runGet
  , read
  , isEmpty

  , CanPut
  , runPut
  , write
  )
where

import           Data.Bifunctor (bimap)
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary.Get
import qualified Data.Binary.IO.Lifted as Lifted
import qualified Data.ByteString as ByteString
import           Prelude hiding (read)
import           System.IO (Handle)

-- * Reader

-- | Alias for 'Lifted.Reader' 'IO'
--
-- @since 0.0.1
newtype Reader = Reader
  { unReader :: Lifted.Reader IO }

instance Lifted.CanGet Reader IO where
  runGet = runGet . unReader

-- | Unlifted version of 'Lifted.newReader'
--
-- @since 0.0.1
newReader
  :: Handle -- ^ Handle that will be read from
  -> IO Reader
newReader handle =
  Reader <$> Lifted.newReader handle

-- | Unlifted version of 'Lifted.newReaderWith'.
--
-- @since 0.1.1
newReaderWith
  :: IO ByteString.ByteString -- ^ Chunk producer
  -> IO Reader
newReaderWith get =
  Reader <$> Lifted.newReaderWith get

-- * Writer

-- | @since 0.0.1
newtype Writer = Writer
  { unWriter :: Lifted.Writer IO }

instance Lifted.CanPut Writer IO where
  runPut = runPut . unWriter

-- | Unlifted version of 'Lifted.newWriter'
--
-- @since 0.0.1
newWriter
  :: Handle -- ^ Handle that will be written to
  -> Writer
newWriter =
  Writer . Lifted.newWriter

-- | Unlifted version of 'Lifted.newWriterWith'
--
-- @since 0.1.1
newWriterWith
  :: (ByteString.ByteString -> IO ()) -- ^ Chunk handler
  -> Writer
newWriterWith =
  Writer . Lifted.newWriterWith

-- * Pipe

-- | Unlifted version of 'Lifted.newPipe'
--
-- @since 0.2.0
newPipe :: IO (Reader, Writer)
newPipe = bimap Reader Writer <$> Lifted.newPipe

-- * Duplex

-- | @since 0.0.1
data Duplex = Duplex
  { duplexWriter :: !Writer
  , duplexReader :: !Reader
  }

instance Lifted.CanGet Duplex IO where
  runGet = runGet . duplexReader

instance Lifted.CanPut Duplex IO where
  runPut = runPut . duplexWriter

-- | Unlifted version of 'Lifted.newDuplex'
--
-- @since 0.0.1
newDuplex
  :: Handle -- ^ Handle that will be read from and written to
  -> IO Duplex
newDuplex handle =
  Duplex (newWriter handle) <$> newReader handle

-- | Unlifted version of 'Lifted.newDuplexWith'
--
-- @since 0.1.1
newDuplexWith
  :: IO ByteString.ByteString
  -> (ByteString.ByteString -> IO ())
  -> IO Duplex
newDuplexWith get push =
  Duplex (newWriterWith push) <$> newReaderWith get

-- * Classes

-- | Alias for 'Lifted.CanGet' @r@ 'IO'
--
-- @since 0.0.1
class Lifted.CanGet r IO => CanGet r

instance Lifted.CanGet r IO => CanGet r

-- | Unlifted version of 'Lifted.runGet'
--
-- @since 0.0.1
runGet
  :: CanGet r
  => r -- ^ Reader / source
  -> Binary.Get a -- ^ Operation to execute
  -> IO a
runGet =
  Lifted.runGet

-- | Unlifted version of 'Lifted.read'
--
-- @since 0.0.1
read
  :: (CanGet r, Binary.Binary a)
  => r -- ^ Read source
  -> IO a
read reader =
  runGet reader Binary.get

-- | Unlifted version of 'Lifted.isEmpty'
--
-- @since 0.3.0
isEmpty :: CanGet r => r -> IO Bool
isEmpty reader = runGet reader Binary.Get.isEmpty

-- | @w@ can execute 'Binary.Put' operations
--
-- @since 0.0.1
class Lifted.CanPut w IO => CanPut w where

instance Lifted.CanPut w IO => CanPut w where

-- | Unlifted version of 'Lifted.runPut'
--
-- @since 0.0.1
runPut
  :: CanPut w
  => w -- ^ Writer / target
  -> Binary.Put -- ^ Operation to execute
  -> IO ()
runPut = Lifted.runPut

-- | Unlifted version of 'Lifted.write'
--
-- @since 0.0.1
write
  :: (CanPut w, Binary.Binary a)
  => w -- ^ Write target
  -> a -- ^ Value to be written
  -> IO ()
write =
  Lifted.write
