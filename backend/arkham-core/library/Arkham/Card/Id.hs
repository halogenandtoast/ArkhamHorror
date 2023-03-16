module Arkham.Card.Id where

import Arkham.Prelude
import Data.UUID (nil)

newtype CardId = CardId { unCardId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, Random)

nullCardId :: CardId
nullCardId = CardId nil
{-# INLINE nullCardId #-}

newtype CommittedCardId = CommittedCardId { unCommittedCardId :: CardId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype HandCardId = HandCardId { unHandCardId :: CardId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype DeckCardId = DeckCardId { unDeckCardId :: CardId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
