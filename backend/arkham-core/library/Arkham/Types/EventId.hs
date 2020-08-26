module Arkham.Types.EventId where

import ClassyPrelude
import Data.Aeson
import Data.UUID

newtype EventId = EventId { unEventId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
