module Arkham.Types.EventId where

import Arkham.Prelude

import Arkham.Types.Card.Id

newtype EventId = EventId { unEventId :: CardId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Random)
