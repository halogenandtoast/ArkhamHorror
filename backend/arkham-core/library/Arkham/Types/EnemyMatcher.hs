module Arkham.Types.EnemyMatcher where

import Arkham.Prelude

import Arkham.Types.EnemyId

data EnemyMatcher
  = EnemyWithTitle Text
  | EnemyWithFullTitle Text Text
  | EnemyWithId EnemyId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
