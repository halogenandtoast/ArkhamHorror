module Arkham.Card.Id (CardId, nullCardId, unsafeMakeCardId) where

import Arkham.Prelude
import Data.UUID (nil)

newtype CardId = CardId UUID
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

-- exports the constructor, but we only want to use this in CardGen
unsafeMakeCardId :: UUID -> CardId
unsafeMakeCardId = CardId

nullCardId :: CardId
nullCardId = CardId nil
{-# INLINE nullCardId #-}
