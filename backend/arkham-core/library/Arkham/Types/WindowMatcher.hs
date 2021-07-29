{-# LANGUAGE PatternSynonyms #-}
module Arkham.Types.WindowMatcher where

import Arkham.Prelude

import Arkham.Types.GameValue
import Arkham.Types.Trait

data WindowMatcher
  = AfterEnemyDefeated Who WindowEnemyMatcher
  | AfterSkillTestResult Who SkillTestMatcher SkillTestResultMatcher
  | WhenWouldHaveSkillTestResult Who SkillTestMatcher SkillTestResultMatcher
  | WhenEnemySpawns Where WindowEnemyMatcher
  | FastPlayerWindow Who
  | AfterTurnBegins Who
  | DuringTurn Who
  | OrWindowMatcher [WindowMatcher]
  | DealtDamageOrHorror Who
  | WhenDrawEncounterCard Who EncounterCardMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data EncounterCardMatcher = NonWeakness
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

pattern EliteEnemy :: WindowEnemyMatcher
pattern EliteEnemy <- EnemyWithTrait Elite where
  EliteEnemy = EnemyWithTrait Elite

pattern NonEliteEnemy :: WindowEnemyMatcher
pattern NonEliteEnemy <- EnemyWithoutTrait Elite where
  NonEliteEnemy = EnemyWithoutTrait Elite

data WindowEnemyMatcher
  = EnemyWithoutTrait Trait
  | EnemyWithTrait Trait
  | AnyEnemy
  | EnemyAtYourLocation
  | EnemyMatchers [WindowEnemyMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup WindowEnemyMatcher where
  EnemyMatchers xs <> EnemyMatchers ys = EnemyMatchers (xs <> ys)
  EnemyMatchers xs <> x = EnemyMatchers (x : xs)
  x <> EnemyMatchers xs = EnemyMatchers (x : xs)
  x <> y = EnemyMatchers [x, y]

type Who = WindowInvestigatorMatcher

data WindowInvestigatorMatcher = You | Anyone
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

type Where = WindowLocationMatcher

data WindowLocationMatcher = YourLocation | Anywhere
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data SkillTestMatcher = WhileInvestigating | AnySkillTest
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data SkillTestResultMatcher = FailureResult ValueMatcher | SuccessResult ValueMatcher | AnyResult
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data ValueMatcher = LessThan (GameValue Int) | AnyValue
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
