{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Matcher.Enemy where

import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Card.CardCode
import Arkham.Card.Id
import Arkham.Criteria.Override
import Arkham.Deck
import {-# SOURCE #-} Arkham.Enemy.Types (Enemy)
import Arkham.Field
import Arkham.GameValue
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
import {-# SOURCE #-} Arkham.Matcher.Treachery
import Arkham.Matcher.Value
import {-# SOURCE #-} Arkham.Modifier
import {-# SOURCE #-} Arkham.Placement
import Arkham.Prelude
import {-# SOURCE #-} Arkham.Source
import Arkham.Token
import Arkham.Trait (Trait (Criminal, Cultist, Ghoul, Humanoid))
import Arkham.Zone
import Control.Lens.Plated (Plated)
import Data.Aeson.TH
import GHC.OverloadedLabels
import GHC.Records

instance IsMatcher EnemyMatcher
instance Be EnemyId EnemyMatcher where
  be = EnemyWithId

data PreyMatcher
  = Prey InvestigatorMatcher
  | OnlyPrey PreyMatcher
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
  | EnemyWithTokens GameValue Token
  | EnemyAt LocationMatcher
  | EnemyWasAt LocationMatcher
  | EnemyAttachedToAsset AssetMatcher
  | EnemyAttachedTo TargetMatcher
  | EnemyCanEnter LocationMatcher
  | EnemyCanSpawnIn LocationMatcher
  | EnemyWantsToSpawnIn LocationMatcher
  | EnemyWithoutSpawn
  | EnemyDrawnFrom DeckSignifier
  | EnemyCanMove
  | EnemyWillMoveWith InvestigatorMatcher
  | EnemyWithSealedChaosTokens Int ChaosTokenMatcher
  | EnemyWithoutTrait Trait
  | EnemyWithKeyword Keyword
  | EnemyWithAnyKey
  | EnemyWithClues ValueMatcher
  | EnemyWithEqualFields (Field Enemy Int) (Field Enemy Int)
  | EnemyWithNonZeroField (Field Enemy Int)
  | EnemyWithMaybeFieldLessThanOrEqualToThis EnemyId (Field Enemy (Maybe Int))
  | EnemyWithMaybeFieldLessThanOrEqualTo Int (Field Enemy (Maybe Int))
  | EnemyWithRemainingHealth ValueMatcher
  | EnemyWithDamage ValueMatcher
  | EnemyWithDoom ValueMatcher
  | EnemyWithMostDoom EnemyMatcher
  | EnemyWithMostClues EnemyMatcher
  | EnemyIsEngagedWith InvestigatorMatcher
  | EnemyWithAsset AssetMatcher
  | EnemyWithoutAttachedEncounterCard
  | EnemyWithAttachedEvent EventMatcher
  | EnemyWithAttachedAsset AssetMatcher
  | EnemyWithAttachedTreachery TreacheryMatcher
  | EnemyWithScarletKey ScarletKeyMatcher
  | FarthestEnemyFrom InvestigatorId EnemyMatcher
  | FarthestEnemyFromAll EnemyMatcher
  | NearestEnemyTo InvestigatorId EnemyMatcher
  | NearestEnemyToFallback InvestigatorId EnemyMatcher
  | NearestEnemyToLocation LocationId EnemyMatcher
  | NearestEnemyToLocationMatch LocationMatcher EnemyMatcher
  | NearestEnemyToLocationFallback LocationId EnemyMatcher
  | NearestEnemyToAnInvestigator EnemyMatcher
  | EnemyIs CardCode
  | EnemyIsExact CardCode
  | EnemyWithCardId CardId
  | AnyEnemy
  | EnemyCanAttack InvestigatorMatcher
  | AttackingEnemy
  | AttackedYouSinceTheEndOfYourLastTurn
  | CanFightEnemy Source
  | CanFightEnemyWith SourceMatcher
  | CanEvadeEnemy Source -- This checks for an ability
  | EnemyCanBeEvadedBy Source -- This is not checking for an ability
  | EnemyCanBeDefeatedBy Source
  | EnemyCanBeRemovedBy Source
  | EnemyCanBeMovedBy Source
  | EnemyCanBeDisengagedBy Source
  | EnemyCanBeEngagedBy Source
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
  | EnemyWithEvadeValue Int
  | EnemyWithFight
  | UnengagedEnemy
  | UniqueEnemy
  | NotEnemy EnemyMatcher
  | MovingEnemy
  | EvadingEnemy
  | AttackedEnemy
  | OnlyEnemyEngagedWith InvestigatorMatcher
  | EnemyCanEngage InvestigatorMatcher
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
  | EnemyIfReturnTo EnemyMatcher EnemyMatcher
  | EnemyWithAnyCardsUnderneath
  | SignatureEnemy
  | EnemyHiddenInHand InvestigatorMatcher
  | EnemyWithConcealed
  | EnemyWithHorrorValue
  | -- | Must be replaced
    ThatEnemy
  deriving stock (Show, Eq, Ord, Data)

instance HasField "canDamage" Source EnemyMatcher where
  getField = EnemyCanBeDamagedBySource

enemy_ :: EnemyMatcher -> EnemyMatcher
enemy_ = id

enemyCanBeEvadedBy :: Sourceable source => source -> EnemyMatcher
enemyCanBeEvadedBy = EnemyCanBeEvadedBy . toSource

instance IsString EnemyMatcher where
  fromString = EnemyWithTitle . fromString

instance Plated EnemyMatcher

instance IsLabel "swarming" EnemyMatcher where
  fromLabel = SwarmingEnemy

instance IsLabel "exhausted" EnemyMatcher where
  fromLabel = ExhaustedEnemy

instance IsLabel "ready" EnemyMatcher where
  fromLabel = ReadyEnemy

instance IsLabel "unengaged" EnemyMatcher where
  fromLabel = UnengagedEnemy

instance IsLabel "humanoid" EnemyMatcher where
  fromLabel = EnemyWithTrait Humanoid

instance IsLabel "ghoul" EnemyMatcher where
  fromLabel = EnemyWithTrait Ghoul

instance IsLabel "criminal" EnemyMatcher where
  fromLabel = EnemyWithTrait Criminal

instance IsLabel "cultist" EnemyMatcher where
  fromLabel = EnemyWithTrait Cultist

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

-- | True when knowing an enemy is merely "any in-play enemy" already guarantees it
-- matches the matcher (the matcher adds no further restriction). Used to decide whether
-- non-enemy fight targets that are attackable "as if an enemy" (Mist-Pylons, Key Loci)
-- should be offered -- they only make sense for an unrestricted fight, not one narrowed
-- by e.g. @EnemyCanAttack You@.
coveredByAnyInPlayEnemy :: EnemyMatcher -> Bool
coveredByAnyInPlayEnemy = \case
  AnyEnemy -> True
  InPlayEnemy m -> coveredByAnyInPlayEnemy m
  EnemyOneOf ms -> any coveredByAnyInPlayEnemy ms
  EnemyMatchAll ms -> all coveredByAnyInPlayEnemy ms
  _ -> False

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
  [ deriveToJSON defaultOptions ''PreyMatcher
  , [d|
      instance FromJSON PreyMatcher where
        parseJSON value = $(mkParseJSON defaultOptions ''PreyMatcher) value <|> (Prey <$> parseJSON value)
    |]
  , deriveToJSON defaultOptions ''EnemyMatcher
  , deriveJSON defaultOptions ''EnemyAttackMatcher
  ]

instance FromJSON EnemyMatcher where
  parseJSON = withObject "EnemyMatcher" $ \o -> do
    tag :: Text <- o .: "tag"
    case tag of
      "EnemyWithTokens" -> do
        contents <- (Right <$> o .: "contents") <|> (Left <$> o .: "contents")
        case contents of
          Left (n, token) -> pure $ EnemyWithTokens (Static n) token
          Right (gv, token) -> pure $ EnemyWithTokens gv token
      _ -> $(mkParseJSON defaultOptions ''EnemyMatcher) (Object o)
