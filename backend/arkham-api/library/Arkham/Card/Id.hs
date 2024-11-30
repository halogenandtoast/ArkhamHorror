module Arkham.Card.Id (CardId, nullCardId, unsafeMakeCardId, unsafeCardIdToInt, unsafeFromCardId, unsafeToCardId) where

import Arkham.Prelude

newtype CardId = CardId Int
  deriving stock Data
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Ord)

-- exports the constructor, but we only want to use this in CardGen
unsafeMakeCardId :: Int -> CardId
unsafeMakeCardId = CardId
{-# INLINE unsafeMakeCardId #-}

unsafeCardIdToInt :: CardId -> Int
unsafeCardIdToInt (CardId cid) = cid
{-# INLINE unsafeCardIdToInt #-}

unsafeFromCardId :: Coercible Int a => CardId -> a
unsafeFromCardId (CardId cid) = coerce cid
{-# INLINE unsafeFromCardId #-}

unsafeToCardId :: forall a. Coercible a Int => a -> CardId
unsafeToCardId a = coerce (coerce @a @Int a)
{-# INLINE unsafeToCardId #-}

nullCardId :: CardId
nullCardId = CardId 0
{-# INLINE nullCardId #-}
