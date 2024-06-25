module Arkham.Card.Id (CardId, nullCardId, unsafeMakeCardId, unsafeCardIdToUUID, unsafeFromCardId) where

import Arkham.Prelude
import Data.UUID (nil)

newtype CardId = CardId UUID
  deriving stock (Data)
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord)

-- exports the constructor, but we only want to use this in CardGen
unsafeMakeCardId :: UUID -> CardId
unsafeMakeCardId = CardId

unsafeCardIdToUUID :: CardId -> UUID
unsafeCardIdToUUID (CardId uuid) = uuid

unsafeFromCardId :: Coercible UUID a => CardId -> a
unsafeFromCardId (CardId uuid) = coerce uuid

nullCardId :: CardId
nullCardId = CardId nil
{-# INLINE nullCardId #-}
