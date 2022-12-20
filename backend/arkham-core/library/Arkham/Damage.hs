module Arkham.Damage where

import Arkham.Prelude

data DamageType = HorrorType | DamageType
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

