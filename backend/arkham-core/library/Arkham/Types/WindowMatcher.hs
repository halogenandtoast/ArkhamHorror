module Arkham.Types.WindowMatcher where

import Arkham.Prelude

import Arkham.Types.Keyword
import Arkham.Types.Trait

data WindowMatcher
  = AfterEnemyDefeated Who WindowEnemyMatcher
  | AfterEnemyEvaded Who WindowEnemyMatcher
  | AfterSkillTest Who WindowSkillTestMatcher
  | AfterEntersPlay What
  | AfterDrawCard Who WindowCardMatcher
  | AfterPlayCard Who WindowCardMatcher
  | WhenWouldFailSkillTest Who WindowSkillTestMatcher
  | WhenWouldRevealChaosToken Who WindowSkillTestMatcher
  | AfterRevealToken Who WindowTokenMatcher
  | WhenTurnBegins Who
  | AfterTurnBegins Who
  | DuringTurn Who
  | DuringFast
  | PhaseBegins WindowPhaseMatcher
  | WhenAllDrawEncounterCard
  | WhenEnemyAttacks Who WindowEnemyMatcher
  | WhenEnemySpawns Where WindowEnemyMatcher
  | WhenEnemyDefeated Who WindowEnemyMatcher
  | WhenDrawTreachery Who WindowTreacheryMatcher
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data What = Self
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data WindowPhaseMatcher = AnyPhase
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data WindowTreacheryMatcher = AnyTreachery | TreacheryWithoutKeyword Keyword
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data WindowTokenMatcher = TokenWithNegativeModifier
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data WindowEnemyMatcher
  = EnemyMatchingAll [WindowEnemyMatcher]
  | EnemyAt Where
  | NonWeaknessEnemy
  | EnemyWithoutTrait Trait
  | EnemyWithTrait Trait
  | EnemyEngagedWith Who
  | AnyEnemy
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data WindowCardMatcher = AnyCard | CardWithTrait Trait
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data WindowSkillTestMatcher = FailedByNoMoreThan Int | AttackingEnemy WindowEnemyMatcher | WhileInvestigating WindowLocationMatcher | AnySkillTest | SkillTestMatchingAll [WindowSkillTestMatcher]
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

type Where = WindowLocationMatcher

data WindowLocationMatcher = YourLocation | ConnectedLocation | Anywhere
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

type Who = WindowInvestigatorMatcher

data WindowInvestigatorMatcher = You | AnotherInvestigator | InvestigatorAtYourLocation | InvestigatorAtAConnectedLocation | Anyone
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)
