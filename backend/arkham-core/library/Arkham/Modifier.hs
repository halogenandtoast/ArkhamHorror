{-# LANGUAGE TemplateHaskell #-}

module Arkham.Modifier (
  Modifier (..),
  ModifierType (..),
  ActionTarget (..),
  setActiveDuringSetup,
  _SearchDepth,
  _AdditionalTargets,
) where

import Arkham.Prelude

import Arkham.Action
import Arkham.Action.Additional
import {-# SOURCE #-} Arkham.Card
import Arkham.Card.CardCode
import Arkham.Card.CardType
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.ClassSymbol
import {-# SOURCE #-} Arkham.Cost
import Arkham.Criteria.Override
import {-# SOURCE #-} Arkham.Enemy.Types
import Arkham.Field
import Arkham.Id
import Arkham.Json
import Arkham.Keyword
import {-# SOURCE #-} Arkham.Matcher.Types
import Arkham.Phase
import Arkham.Scenario.Deck
import Arkham.SkillType
import Arkham.SlotType
import {-# SOURCE #-} Arkham.Source
import {-# SOURCE #-} Arkham.Strategy
import {-# SOURCE #-} Arkham.Target
import Arkham.Trait
import Control.Lens (Prism', prism')
import Data.Aeson.TH
import GHC.OverloadedLabels

data ModifierType
  = ActionCostOf ActionTarget Int
  | BountiesOnly
  | CommitCost Cost
  | AbilityModifier Target Int ModifierType
  | SkillTestResultValueModifier Int
  | TraitRestrictedModifier Trait ModifierType
  | ActionCostModifier Int
  | ActionCostSetToModifier Int
  | ActionSkillModifier {action :: Action, skillType :: SkillType, value :: Int}
  | ActionsAreFree
  | AddKeyword Keyword
  | AddTrait Trait
  | AddSkillIcons [SkillIcon]
  | RemoveSkillIcons [SkillIcon]
  | AdditionalActions Int
  | FewerActions Int
  | GiveAdditionalAction AdditionalAction
  | AdditionalStartingUses Int
  | SetAbilityCost Cost
  | AdditionalCost Cost
  | ChangeRevealStrategy RevealStrategy
  | CannotTriggerAbilityMatching AbilityMatcher
  | ConnectedToWhen LocationMatcher LocationMatcher
  | AlternateSuccessfullEvasion
  | AlternateSuccessfullInvestigation
  | AlternativeReady Source
  | AnySkillValue Int
  | AsIfInHand Card
  | AsIfAt LocationId
  | AsIfEngagedWith EnemyId
  | AsIfUnderControlOf InvestigatorId
  | AttacksCannotBeCancelled
  | BaseSkillOf {skillType :: SkillType, value :: Int}
  | BecomesFast
  | Blank
  | Blocked
  | IsEmptySpace
  | CanEnterEmptySpace
  | CannotEnter LocationId
  | CanAssignDamageToAsset AssetId
  | CanAssignHorrorToAsset AssetId
  | CanBeAssignedDirectDamage
  | CanBeFoughtAsIfAtYourLocation
  | CanBecomeFast CardMatcher
  | CanBecomeFastOrReduceCostOf CardMatcher Int -- Used by Chuck Fergus (2), check for notes
  | RevealChaosTokensBeforeCommittingCards
  | CanCommitToSkillTestPerformedByAnInvestigatorAt LocationMatcher
  | CanOnlyBeAttackedByAbilityOn (Set CardCode)
  | CanOnlyUseCardsInRole ClassSymbol
  | CanPlayTopOfDiscard (Maybe CardType, [Trait])
  | CanPlayTopOfDeck CardMatcher
  | CanSpendResourcesOnCardFromInvestigator InvestigatorMatcher CardMatcher
  | CancelSkills
  | CannotParleyWith EnemyMatcher
  | CannotAttack
  | CannotBeAttacked
  | CannotBeFlipped
  | CannotBeDefeated
  | CanOnlyBeDefeatedBy Source
  | CanOnlyBeDefeatedByDamage
  | CancelAttacksByEnemies Card EnemyMatcher
  | CannotBeDamaged
  | CannotBeDamagedByPlayerSources SourceMatcher
  | CannotBeDamagedByPlayerSourcesExcept SourceMatcher
  | CannotBeEnteredBy EnemyMatcher
  | CannotBeEvaded
  | CannotBeRevealed
  | CannotCancelHorror
  | CannotCancelOrIgnoreChaosToken ChaosTokenFace
  | CannotCommitCards CardMatcher
  | CannotDiscoverClues
  | CannotDrawCards
  | CannotEngage InvestigatorId
  | CannotBeEngaged
  | CannotBeEngagedBy EnemyMatcher
  | CannotBeAttackedBy EnemyMatcher
  | CannotGainResources
  | CannotHealHorror
  | CannotHealHorrorOnOtherCards Target
  | CannotExplore
  | CannotInvestigate
  | CannotInvestigateLocation LocationId
  | CannotMakeAttacksOfOpportunity
  | CannotManipulateDeck
  | ActionDoesNotCauseAttacksOfOpportunity Action
  | CannotReady
  | DoesNotReadyDuringUpkeep
  | CannotFight EnemyMatcher
  | CannotEvade EnemyMatcher
  | CannotMove
  | CannotBeMoved
  | CannotDisengageEnemies
  | CannotMoveMoreThanOnceEachTurn
  | Mulligans Int
  | CannotMulligan
  | CannotReplaceWeaknesses
  | CannotPerformSkillTest
  | CannotPlaceClues
  | CannotPlay CardMatcher
  | CannotPutIntoPlay CardMatcher
  | CannotSpendClues
  | EffectsCannotBeCanceled
  | MaxCluesDiscovered Int
  | CannotDiscoverCluesAt LocationMatcher
  | CannotTakeAction ActionTarget
  | MustTakeAction ActionTarget
  | CannotTakeControlOfClues
  | NoMoreThanOneDamageOrHorrorAmongst AssetMatcher
  | CannotTriggerFastAbilities
  | CardsCannotLeaveYourDiscardPile
  | ChangeChaosTokenModifier ChaosTokenModifier
  | ControlledAssetsCannotReady
  | DamageDealt Int
  | DamageTaken Int
  | Difficulty Int
  | DiscoveredClues Int
  | DoNotRemoveDoom
  | DoNotTakeUpSlot SlotType
  | SlotCanBe SlotType SlotType
  | CannotPlaceDoomOnThis
  | DoNotDrawChaosTokensForSkillChecks
  | DoesNotDamageOtherInvestigator
  | DoomThresholdModifier Int
  | DoomSubtracts
  | DoubleDifficulty
  | DoubleNegativeModifiersOnChaosTokens
  | DoubleSkillIcons
  | DoubleSuccess
  | DoubleBaseSkillValue
  | DuringEnemyPhaseMustMoveToward Target
  | EnemyCannotEngage InvestigatorId
  | EnemyEvade Int
  | EnemyFight Int
  | EnemyEvadeWithMin Int (Min Int)
  | EnemyFightWithMin Int (Min Int)
  | AsIfEnemyFight Int
  | AlternateFightField (SomeField Enemy)
  | AlternateEvadeField (SomeField Enemy)
  | CountsAsInvestigatorForHunterEnemies
  | FailTies
  | FewerSlots SlotType Int
  | ForcedChaosTokenChange ChaosTokenFace [ChaosTokenFace]
  | ForcePrey PreyMatcher
  | HandSize Int
  | IgnoreHandSizeReduction
  | HandSizeCardCount Int
  | HealthModifier Int
  | HealthModifierWithMin Int (Min Int)
  | HealHorrorOnThisAsIfInvestigator InvestigatorId
  | HorrorDealt Int
  | HunterConnectedTo LocationId
  | CanRetaliateWhileExhausted
  | IgnoreAllCosts
  | IgnoreRetaliate
  | IgnoreAloof
  | IgnoreText
  | IgnoreChaosToken
  | IgnoreChaosTokenEffects
  | IncreaseCostOf CardMatcher Int
  | KilledIfDefeated
  | MaxDamageTaken Int
  | MayChooseNotToTakeUpkeepResources
  | ModifierIfSucceededBy Int Modifier
  | NegativeToPositive
  | NoDamageDealt
  | NonDirectHorrorMustBeAssignToThisFirst
  | PlaceOnBottomOfDeckInsteadOfDiscard
  | ReduceCostOf CardMatcher Int
  | CanReduceCostOf CardMatcher Int
  | RemoveFromGameInsteadOfDiscard
  | RemoveKeyword Keyword
  | ReturnToHandAfterTest
  | SanityModifier Int
  | SetDifficulty Int
  | ShroudModifier Int
  | SetShroud Int
  | SkillModifier {skillType :: SkillType, value :: Int}
  | AddSkillValue SkillType
  | SkillCannotBeIncreased SkillType
  | SkipMythosPhaseStep MythosPhaseStep
  | SpawnNonEliteAtConnectingInstead
  | ForceSpawnLocation LocationMatcher
  | CannotSpawnIn LocationMatcher
  | StartingHand Int
  | StartingResources Int
  | StartingClues Int
  | ChaosTokenFaceModifier [ChaosTokenFace]
  | ChaosTokenValueModifier Int
  | TopCardOfDeckIsRevealed
  | TreatAllDamageAsDirect
  | TreatRevealedChaosTokenAs ChaosTokenFace
  | UseSkillInPlaceOf SkillType SkillType
  | UseSkillInsteadOf SkillType SkillType
  | SkillModifiersAffectOtherSkill SkillType SkillType
  | AddSkillToOtherSkill SkillType SkillType
  | XPModifier Int
  | SkillTestAutomaticallySucceeds
  | IgnoreRevelation
  | GainVictory Int
  | InVictoryDisplayForCountingVengeance
  | EnemyFightActionCriteria CriteriaOverride
  | EnemyEvadeActionCriteria CriteriaOverride
  | CanPlayWithOverride CriteriaOverride
  | SetAbilityCriteria CriteriaOverride
  | RevealAnotherChaosToken -- TODO: Only ShatteredAeons handles this, if a player card affects this, all scenarios have to be updated
  | IgnoreLimit
  | CanIgnoreLimit
  | DoNotExhaustEvaded
  | DoNotDisengageEvaded
  | CannotBeAdvancedByDoomThreshold
  | MetaModifier Value
  | CanModify ModifierType
  | NoSurge
  | UseEncounterDeck ScenarioEncounterDeckKey -- The Wages of Sin
  | Omnipotent
  | CountAllDoomInPlay
  | SetAfterPlay AfterPlayStrategy
  | SearchDepth Int
  | AdditionalTargets Int
  | MayIgnoreLocationEffectsAndKeywords
  deriving stock (Show, Eq, Ord, Data)

_SearchDepth :: Prism' ModifierType Int
_SearchDepth = prism' SearchDepth $ \case
  SearchDepth n -> Just n
  _ -> Nothing

_AdditionalTargets :: Prism' ModifierType Int
_AdditionalTargets = prism' AdditionalTargets $ \case
  AdditionalTargets n -> Just n
  _ -> Nothing

data Modifier = Modifier
  { modifierSource :: Source
  , modifierType :: ModifierType
  , modifierActiveDuringSetup :: Bool
  }
  deriving stock (Show, Eq, Ord, Data)

data ActionTarget
  = FirstOneOfPerformed [Action]
  | IsAction Action
  | EnemyAction Action EnemyMatcher
  deriving stock (Show, Eq, Ord, Data)

instance IsLabel "draw" ActionTarget where
  fromLabel = IsAction #draw

instance IsLabel "investigate" ActionTarget where
  fromLabel = IsAction #investigate

setActiveDuringSetup :: Modifier -> Modifier
setActiveDuringSetup m = m {modifierActiveDuringSetup = True}

$(deriveJSON defaultOptions ''ActionTarget)
$( do
    deriveModifierType <- deriveJSON defaultOptions ''ModifierType
    deriveModifier <- deriveJSON (aesonOptions $ Just "modifier") ''Modifier
    pure $ deriveModifierType ++ deriveModifier
 )
