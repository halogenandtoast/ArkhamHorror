module Arkham.Resolution where

import Arkham.Prelude

data Resolution = NoResolution | Resolution Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable, ToJSONKey, FromJSONKey, NoThunks)
