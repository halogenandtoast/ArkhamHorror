module Arkham.Types.Direction
  ( module Arkham.Types.Direction
  ) where

import Arkham.Prelude

data Direction = Above | Below | LeftOf | RightOf
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable, ToJSONKey, FromJSONKey)
