module Arkham.Types.Card.Id where

import ClassyPrelude
import Data.Aeson
import Data.UUID

newtype CardId = CardId { unCardId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype CommitedCardId = CommitedCardId { unCommitedCardId :: CardId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype DeckCardId = DeckCardId { unDeckCardId :: CardId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
