module Arkham.Types.WindowMatcher where

import Arkham.Prelude

import Arkham.Types.Trait

data WindowMatcher
  = AfterEnemyDefeated Who WindowEnemyMatcher
  | WhenEnemySpawns Where WindowEnemyMatcher
  | FastPlayerWindow Who
  | OrWindowMatcher [WindowMatcher]
  | DealtDamageOrHorror Who
  | WhenDrawEncounterCard Who EncounterCardMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data EncounterCardMatcher = NonWeakness
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data WindowEnemyMatcher = EnemyWithTrait Trait | AnyEnemy
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

type Who = WindowInvestigatorMatcher

data WindowInvestigatorMatcher = You | Anyone
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

type Where = WindowLocationMatcher

data WindowLocationMatcher = YourLocation | Anywhere
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
