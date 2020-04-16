{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Communication.Message 
  ( Message
  , toMessage
  , fromMessage
  )
where

import GHC.Generics (Generic)

import qualified Control.Monad.Error.Class as Error
import           Control.Monad (when)

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Binary as Binary
import qualified Data.Hashable as Hashable
import qualified Data.Binary.Get as Binary.Get
import qualified Data.Binary.Put as Binary.Put
import qualified Data.Typeable as Typeable
import           Data.Maybe (isNothing)

import qualified Type.Reflection as Reflection

data Message = Message
  { messageType :: !Reflection.SomeTypeRep
  , messageBody :: ByteString.ByteString
  }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (Hashable.Hashable, Binary.Binary)

-- | Pack a message manually using a given message type and an encode operation.
toMessageWith 
  :: Reflection.SomeTypeRep -- ^ Message type
  -> Binary.Put -- ^ Encode operation
  -> Message
toMessageWith typeRep put = Message
  { messageType = typeRep
  , messageBody = Binary.Put.runPut put
  }

-- | Pack a message.
toMessage 
  :: (Reflection.Typeable a, Binary.Binary a) 
  => a -- ^ Value to pack into the message
  -> Message
toMessage value = toMessageWith (Typeable.typeOf value) (Binary.put value)

-- | An error that can occur while unpacking a message.
data MessageError
  = MessageTypeMismatch
    { messageErrorSourceType :: Reflection.SomeTypeRep
      -- ^ Type in the 'Message'
    , messageErrorTargetType :: Reflection.SomeTypeRep
      -- ^ Target type
    }
  | MessageGetError
    { messageErrorBodyRemaining :: ByteString.ByteString
      -- ^ Remaining body that wasn't parsed
    , messageErrorBodyOffset :: Binary.Get.ByteOffset
      -- ^ Offset into the body at which the error occured
    , messageErrorMessage :: String
      -- ^ Error message
    }

-- | Do something with the message type in form of a 'Reflection.TypeRep'.
withMessageType 
  :: Message -- ^ Contains the message type
  -> (forall k (a :: k). Reflection.TypeRep a -> b) -- ^ Operation to perform on the message type
  -> b
withMessageType message f =
  case messageType message of
    Reflection.SomeTypeRep typeRep -> f typeRep

-- | Ensure that message type and target type are the same.
verifyMessageType 
  :: Error.MonadError MessageError m 
  => Message -- ^ Contains the message type
  -> Reflection.TypeRep a -- ^ Target type
  -> m ()
verifyMessageType message targetTypeRep = 
  withMessageType message $ \sourceTypeRep -> 
    when (isNothing (Reflection.eqTypeRep sourceTypeRep targetTypeRep)) $
      Error.throwError MessageTypeMismatch
        { messageErrorSourceType = Reflection.SomeTypeRep sourceTypeRep
        , messageErrorTargetType = Reflection.SomeTypeRep targetTypeRep
        }

-- | Decode the message body using the given 'Binary.Get'. This will not perform any type checking.
decodeMessage
  :: Error.MonadError MessageError m
  => Message -- ^ Contains the message body
  -> Binary.Get a
  -> m a
decodeMessage message getter = do
  case Binary.Get.runGetOrFail getter (messageBody message) of
    Right (_, _, result) -> 
      pure result

    Left (remainingBody, offset, errorMessage) ->
      Error.throwError MessageGetError
        { messageErrorBodyRemaining = remainingBody
        , messageErrorBodyOffset = offset
        , messageErrorMessage = errorMessage
        }

-- | Unpack a message.
fromMessage
  :: forall a m
  .  (Reflection.Typeable a, Binary.Binary a, Error.MonadError MessageError m)
  => Message -- ^ Message containing the value to unpack
  -> m a
fromMessage message = do
  verifyMessageType message (Reflection.typeRep @a)
  decodeMessage message Binary.get
