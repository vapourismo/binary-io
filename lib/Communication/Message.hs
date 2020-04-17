{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Communication.Message
  ( Message (..) )
where

import qualified Control.Monad.Fail as Fail
import           Control.Monad (when)
import           Control.DeepSeq (rnf)

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary.Get
import qualified Data.Binary.Put as Binary.Put
import qualified Data.Hashable as Hashable
import qualified Data.Typeable as Typeable
import           Data.Maybe (isNothing)
import           Data.Bits (finiteBitSize)
import           Data.Int (Int64)

import qualified Type.Reflection as Reflection

-- | Will evaluate to bottom if 'Int' and 'Int64' don't have the same upper bound or have a
-- different bit size.
int64IntConsistency :: ()
int64IntConsistency = rnf
  [ assert (fromIntegral @Int @Int64 maxBound == maxBound) ""
  , assert (fromIntegral @Int64 @Int maxBound == maxBound) ""
  , assert (finiteBitSize @Int maxBound == finiteBitSize @Int64 maxBound) ""
  ]
  where
    assert False message = error message
    assert True  _       = ()

-- | Wrapper for @a@ that makes decoding a little safer
--
-- The 'Binary.Binary' instance will make sure that decoding only succeeds if the
-- message-to-be-decoded was also encoded using the same type.
--
-- Additionally, it will restrict 'Get' operations that would otherwise consume an entire stream
-- only operate on the payload that was actually encoded.
--
newtype Message a = Message
  { messageBody :: a }
  deriving (Show, Eq, Ord, Hashable.Hashable)

instance (Typeable.Typeable a, Binary.Binary a) => Binary.Binary (Message a) where
  put (Message body) = do
    Binary.put (Typeable.typeOf body)

    let payload = Binary.Put.runPut (Binary.put body)
    Binary.Put.putInt64le (ByteString.length payload)
    Binary.Put.putLazyByteString payload

  get = seq int64IntConsistency $ do
    let targetTypeRep = Reflection.typeRep @a
    Reflection.SomeTypeRep sourceTypeRep <- Binary.get

    when (isNothing (Reflection.eqTypeRep sourceTypeRep targetTypeRep)) $
      Fail.fail ("Expected type " <> show targetTypeRep <> ", got " <> show sourceTypeRep)

    length <- Binary.Get.getInt64le
    Message <$> Binary.Get.isolate (fromIntegral length) Binary.get
