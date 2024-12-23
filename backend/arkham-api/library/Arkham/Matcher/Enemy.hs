{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Matcher.Enemy where

import Arkham.Card.CardCode
import Arkham.Card.Id
import Arkham.Criteria.Override
import {-# SOURCE #-} Arkham.Enemy.Types (Enemy)
import Arkham.Field
import Arkham.Id
import Arkham.Keyword (Keyword)
import {-# SOURCE #-} Arkham.Matcher.Asset
import Arkham.Matcher.Base
import Arkham.Matcher.ChaosToken
import {-# SOURCE #-} Arkham.Matcher.Event
import {-# SOURCE #-} Arkham.Matcher.Investigator
import {-# SOURCE #-} Arkham.Matcher.Location
import {-# SOURCE #-} Arkham.Matcher.Source
import {-# SOURCE #-} Arkham.Matcher.Target
import Arkham.Matcher.Value
import {-# SOURCE #-} Arkham.Modifier
import {-# SOURCE #-} Arkham.Placement
import Arkham.Prelude
import {-# SOURCE #-} Arkham.Source
import Arkham.Token
import Arkham.Trait (Trait (Ghoul))
import Arkham.Zone
import Control.Lens.Plated (Plated)
import Data.Aeson.TH
import GHC.OverloadedLabels

instance IsMatcher EnemyMatcher
instance Be EnemyId EnemyMatcher where
  be = EnemyWithId

data PreyMatcher
  = Prey InvestigatorMatcher
  | OnlyPrey InvestigatorMatcher
  | BearerOf EnemyId
  | RestrictedBearerOf EnemyId InvestigatorMatcher
  deriving stock (Show, Eq, Ord, Data)

data EnemyMatcher
  = EnemyWithTitle Text
  | EnemyWithVictory
  | EnemyWithFullTitle Text Text
  | EnemyWithId EnemyId
  | EnemyWithTrait Trait
  | EnemyWithToken Token
  | EnemyAt LocationMatcher
  | EnemyAttachedToAsset AssetMatcher
  | EnemyAttachedTo TargetMatcher
  | EnemyCanEnter LocationMatcher
  | EnemyCanSpawnIn LocationMatcher
  | EnemyCanMove
  | EnemyWithSealedChaosTokens Int ChaosTokenMatcher
  | EnemyWithoutTrait Trait
  | EnemyWithKeyword Keyword
  | EnemyWithAnyKey
  | EnemyWithClues ValueMatcher
  | EnemyWithEqualFields (Field Enemy Int) (Field Enemy Int)
  | EnemyWithNonZeroField (Field Enemy Int)
  | EnemyWithMaybeFieldLessThanOrEqualToThis EnemyId (Field Enemy (Maybe Int))
  | EnemyWithRemainingHealth ValueMatcher
  | EnemyWithDamage ValueMatcher
  | EnemyWithDoom ValueMatcher
  | EnemyWithMostDoom EnemyMatcher
  | EnemyIsEngagedWith InvestigatorMatcher
  | EnemyWithAsset AssetMatcher
  | EnemyWithAttachedEvent EventMatcher
  | EnemyWithAttachedAsset AssetMatcher
  | FarthestEnemyFrom InvestigatorId EnemyMatcher
  | FarthestEnemyFromAll EnemyMatcher
  | NearestEnemyTo InvestigatorId EnemyMatcher
  | NearestEnemyToLocation LocationId EnemyMatcher
  | NearestEnemyToAnInvestigator EnemyMatcher
  | EnemyIs CardCode
  | EnemyWithCardId CardId
  | AnyEnemy
  | EnemyCanAttack InvestigatorMatcher
  | AttackingEnemy
  | AttackedYouSinceTheEndOfYourLastTurn
  | CanFightEnemy Source
  | CanEvadeEnemy Source -- This checks for an ability
  | EnemyCanBeEvadedBy Source -- This is not checking for an ability
  | EnemyCanBeDefeatedBy Source
  | CanFightEnemyWithOverride CriteriaOverride
  | CanEvadeEnemyWithOverride CriteriaOverride -- This checks for an ability but overrides the criteria
  | CanEngageEnemy Source
  | CanEngageEnemyWithOverride CriteriaOverride
  | EnemyDiscardedBy InvestigatorMatcher
  | ReadyEnemy
  | ExhaustedEnemy
  | NonWeaknessEnemy
  | EnemyInHandOf InvestigatorMatcher
  | CanParleyEnemy InvestigatorMatcher
  | EnemyMatchAll [EnemyMatcher]
  | EnemyOwnedBy InvestigatorMatcher
  | EnemyOneOf [EnemyMatcher]
  | EnemyWithMostRemainingHealth EnemyMatcher
  | EnemyWithoutModifier ModifierType
  | EnemyWithModifier ModifierType
  | EnemyWithEvade
  | EnemyWithFight
  | UnengagedEnemy
  | UniqueEnemy
  | NotEnemy EnemyMatcher
  | MovingEnemy
  | EvadingEnemy
  | AttackedEnemy
  | IsIchtacasPrey
  | EnemyCanBeDamagedBySource Source
  | OutOfPlayEnemy OutOfPlayZone EnemyMatcher
  | InPlayEnemy EnemyMatcher
  | IncludeOmnipotent EnemyMatcher
  | IncludeOutOfPlayEnemy EnemyMatcher
  | EnemyWithPlacement Placement
  | EnemyWithBounty -- Tony Morgan
  | PatrolEnemy
  | SwarmOf EnemyId
  | IsSwarm
  | IsHost
  | SwarmingEnemy
  | EnemyWithHealth
  | DefeatedEnemy EnemyMatcher
  | CanBeAttackedBy InvestigatorMatcher
  | EnemyWhenEvent EventMatcher
  | EnemyWhenLocation LocationMatcher
  | EnemyWhenInvestigator InvestigatorMatcher
  | EnemyWhenOtherEnemy EnemyMatcher
  | EnemyWithAnyCardsUnderneath
  | -- | Must be replaced
    ThatEnemy
  deriving stock (Show, Eq, Ord, Data)

enemy_ :: EnemyMatcher -> EnemyMatcher
enemy_ = id

instance Plated EnemyMatcher

instance IsLabel "swarming" EnemyMatcher where
  fromLabel = SwarmingEnemy

instance IsLabel "exhausted" EnemyMatcher where
  fromLabel = ExhaustedEnemy

instance IsLabel "ready" EnemyMatcher where
  fromLabel = ReadyEnemy

instance IsLabel "unengaged" EnemyMatcher where
  fromLabel = UnengagedEnemy

instance IsLabel "ghoul" EnemyMatcher where
  fromLabel = EnemyWithTrait Ghoul

instance Semigroup EnemyMatcher where
  AnyEnemy <> x = x
  x <> AnyEnemy = x
  EnemyMatchAll xs <> EnemyMatchAll ys = EnemyMatchAll (xs <> ys)
  EnemyMatchAll xs <> x = EnemyMatchAll (x : xs)
  x <> EnemyMatchAll xs = EnemyMatchAll (x : xs)
  x <> y = EnemyMatchAll [x, y]

instance Not EnemyMatcher where
  not_ = NotEnemy

instance Monoid EnemyMatcher where
  mempty = AnyEnemy

class IsEnemyMatcher a where
  toEnemyMatcher :: a -> EnemyMatcher

instance IsEnemyMatcher EnemyMatcher where
  toEnemyMatcher = id

instance IsEnemyMatcher EnemyId where
  toEnemyMatcher = EnemyWithId

data EnemyAttackMatcher
  = AnyEnemyAttack
  | AttackOfOpportunityAttack
  | AttackOfOpportunityAttackYouProvoked
  | AttackViaAlert
  | CancelableEnemyAttack EnemyAttackMatcher
  | NotEnemyAttack EnemyAttackMatcher
  | AttackDamagedAsset AssetMatcher
  | AttackDealtDamageOrHorror
  | EnemyAttackMatches [EnemyAttackMatcher]
  | AttackViaSource SourceMatcher
  deriving stock (Show, Eq, Ord, Data)

instance Semigroup EnemyAttackMatcher where
  AnyEnemyAttack <> x = x
  x <> AnyEnemyAttack = x
  EnemyAttackMatches xs <> EnemyAttackMatches ys = EnemyAttackMatches $ xs <> ys
  EnemyAttackMatches xs <> x = EnemyAttackMatches $ xs <> [x]
  x <> EnemyAttackMatches xs = EnemyAttackMatches $ x : xs
  x <> y = EnemyAttackMatches [x, y]

instance Monoid EnemyAttackMatcher where
  mempty = AnyEnemyAttack

instance Not EnemyAttackMatcher where
  not_ = NotEnemyAttack

mconcat
  [ deriveJSON defaultOptions ''PreyMatcher
  , deriveJSON defaultOptions ''EnemyMatcher
  , deriveJSON defaultOptions ''EnemyAttackMatcher
  ]
