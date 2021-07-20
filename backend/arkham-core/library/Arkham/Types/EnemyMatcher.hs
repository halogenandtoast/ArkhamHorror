module Arkham.Types.EnemyMatcher where

import Arkham.Prelude

import Arkham.Types.Id

data EnemyMatcher
  = EnemyWithTitle Text
  | EnemyWithFullTitle Text Text
  | EnemyWithId EnemyId
  | NonEliteEnemy
  | HunterEnemies
  | EnemyAtLocation LocationId
  | EnemyMatchAll [EnemyMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
