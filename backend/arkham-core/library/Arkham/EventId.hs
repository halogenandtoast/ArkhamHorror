module Arkham.EventId where

import Arkham.Prelude

import Arkham.Card.Id

newtype EventId = EventId { unEventId :: CardId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, Random)
