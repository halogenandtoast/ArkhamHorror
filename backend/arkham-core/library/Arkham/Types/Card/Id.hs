module Arkham.Types.Card.Id where

import Arkham.Prelude

newtype CardId = CardId { unCardId :: UUID }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Random)

newtype CommittedCardId = CommittedCardId { unCommittedCardId :: CardId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype HandCardId = HandCardId { unHandCardId :: CardId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype DeckCardId = DeckCardId { unDeckCardId :: CardId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
