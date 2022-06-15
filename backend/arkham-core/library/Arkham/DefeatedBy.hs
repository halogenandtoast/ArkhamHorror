module Arkham.DefeatedBy where

import Arkham.Prelude

data DefeatedBy
  = DefeatedByHorror
  | DefeatedByDamage
  | DefeatedByDamageAndHorror
  | DefeatedByOther
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
