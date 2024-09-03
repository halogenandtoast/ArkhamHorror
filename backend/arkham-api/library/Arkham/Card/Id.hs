module Arkham.Card.Id (CardId, nullCardId, unsafeMakeCardId, unsafeCardIdToUUID, unsafeFromCardId, unsafeToCardId) where

import Arkham.Prelude
import Data.UUID (nil)

newtype CardId = CardId UUID
  deriving stock (Data)
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord)

-- exports the constructor, but we only want to use this in CardGen
unsafeMakeCardId :: UUID -> CardId
unsafeMakeCardId = CardId
{-# INLINE unsafeMakeCardId #-}

unsafeCardIdToUUID :: CardId -> UUID
unsafeCardIdToUUID (CardId uuid) = uuid
{-# INLINE unsafeCardIdToUUID #-}

unsafeFromCardId :: Coercible UUID a => CardId -> a
unsafeFromCardId (CardId uuid) = coerce uuid
{-# INLINE unsafeFromCardId #-}

unsafeToCardId :: forall a. Coercible a UUID => a -> CardId
unsafeToCardId a = coerce (coerce @a @UUID a)
{-# INLINE unsafeToCardId #-}

nullCardId :: CardId
nullCardId = CardId nil
{-# INLINE nullCardId #-}
