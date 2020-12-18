module Arkham.Types.Cost
  ( module Arkham.Types.Cost
  )
where

import Arkham.Types.Action
import Arkham.Types.Trait
import ClassyPrelude

data Cost =
  ActionCost Int (Maybe Action) (HashSet Trait)
