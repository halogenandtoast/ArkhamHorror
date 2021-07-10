module Arkham.Types.EnemyMatcher where

import Arkham.Prelude

import Arkham.Types.EnemyId
import Arkham.Types.Trait

data EnemyMatcher
  = EnemyWithTitle Text
  | EnemyWithFullTitle Text Text
  | EnemyWithId EnemyId
  | EnemyWithTrait Trait
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
