module Arkham.Types.EnemyMatcher where

import Arkham.Prelude

import Arkham.Types.Id

data EnemyMatcher
  = EnemyWithTitle Text
  | EnemyWithFullTitle Text Text
  | EnemyWithId EnemyId
  | NonEliteEnemy
  | EnemyAtLocation LocationId
  | EnemyMatchAll [EnemyMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
