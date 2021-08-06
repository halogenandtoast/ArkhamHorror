module Arkham.Types.Resolution
  ( module Arkham.Types.Resolution
  ) where

import Arkham.Prelude

data Resolution = NoResolution | Resolution Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
