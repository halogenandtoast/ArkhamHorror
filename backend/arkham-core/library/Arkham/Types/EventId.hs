module Arkham.Types.EventId where

import Arkham.Prelude

newtype EventId = EventId { unEventId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
