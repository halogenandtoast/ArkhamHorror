module Arkham.Direction
  ( module Arkham.Direction
  ) where

import Arkham.Prelude

data Direction = Above | Below | LeftOf | RightOf
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable, ToJSONKey, FromJSONKey)
