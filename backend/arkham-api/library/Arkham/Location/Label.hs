module Arkham.Location.Label where

import Arkham.Prelude

newtype LocationLabel = LocationLabel {unLocationLabel :: Text}
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
