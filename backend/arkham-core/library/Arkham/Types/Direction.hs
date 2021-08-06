module Arkham.Types.Direction
  ( module Arkham.Types.Direction
  ) where

import Arkham.Prelude

data Direction = Above | Below | LeftOf | RightOf
  deriving stock (Ord, Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
