module Arkham.Resolution where

import Arkham.Prelude

data Resolution = NoResolution | Resolution Int
  deriving stock (Show, Ord, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON, Hashable, ToJSONKey, FromJSONKey)
