module Arkham.Types.WindowMatcher where

import Arkham.Prelude

import Arkham.Types.Trait

data WindowMatcher = AfterEnemyDefeated Who WindowEnemyMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

newtype WindowEnemyMatcher = EnemyWithTrait Trait
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

type Who = WindowInvestigatorMatcher

data WindowInvestigatorMatcher = You
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
