module Arkham.Types.WindowMatcher where

import Arkham.Prelude

import Arkham.Types.Card.CardMatcher
import Arkham.Types.GameValue
import Arkham.Types.Matcher

data WindowMatcher
  = EnemyDefeated When Who EnemyMatcher
  | EnemyEvaded When Who EnemyMatcher
  | MythosStep WindowMythosStepMatcher
  | EnemyAttacks When Who EnemyMatcher
  | RevealChaosToken When Who WindowTokenMatcher
  | SkillTestResult When Who SkillTestMatcher SkillTestResultMatcher
  | WhenWouldHaveSkillTestResult Who SkillTestMatcher SkillTestResultMatcher
  | WhenEnemySpawns Where EnemyMatcher
  | FastPlayerWindow
  | AfterTurnBegins Who
  | DuringTurn Who
  | OrWindowMatcher [WindowMatcher]
  | DealtDamageOrHorror Who
  | DrawCard When Who CardMatcher
  | PhaseBegins When WindowPhaseMatcher
  | PlayerHasPlayableCard ExtendedCardMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

type When = WindowTimingMatcher

data WindowTimingMatcher = When | After
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SkillTestMatcher = WhileInvestigating | WhileAttackingAnEnemy | AnySkillTest
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SkillTestResultMatcher = FailureResult ValueMatcher | SuccessResult ValueMatcher | AnyResult
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ValueMatcher = LessThan (GameValue Int) | AnyValue
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data WindowTokenMatcher = WithNegativeModifier
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data WindowPhaseMatcher = AnyPhase
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data WindowMythosStepMatcher = WhenAllDrawEncounterCard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
