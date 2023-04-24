{-# LANGUAGE TemplateHaskell #-}
module Arkham.Modifier
  ( Modifier(..)
  , ModifierType(..)
  , ActionTarget(..)
  , setActiveDuringSetup
  ) where

import Arkham.Prelude

import Arkham.Action
import Arkham.Action.Additional
import {-# SOURCE #-} Arkham.Card
import Arkham.Card.CardCode
import Arkham.Card.CardType
import Arkham.ChaosBag.RevealStrategy
import Arkham.ClassSymbol
import Arkham.Criteria.Override
import {-# SOURCE #-} Arkham.Cost
import {-# SOURCE #-} Arkham.Enemy.Types
import Arkham.Field
import Arkham.Id
import Arkham.Json
import Arkham.Keyword
import {-# SOURCE #-} Arkham.Matcher.Types
import Arkham.Phase
import Arkham.SkillType
import Arkham.SlotType
import {-# SOURCE #-} Arkham.Source
import {-# SOURCE #-} Arkham.Target
import Arkham.Token
import Arkham.Trait
import Data.Aeson.TH

data ModifierType
  = ActionCostOf ActionTarget Int
  | CommitCost Cost
  | AbilityModifier Target Int ModifierType
  | SkillTestResultValueModifier Int
  | TraitRestrictedModifier Trait ModifierType
  | ActionCostModifier Int
  | ActionCostSetToModifier Int
  | ActionSkillModifier { action :: Action, skillType :: SkillType, value ::  Int }
  | ActionsAreFree
  | AddKeyword Keyword
  | AddTrait Trait
  | AddSkillIcons [SkillIcon]
  | RemoveSkillIcons [SkillIcon]
  | AdditionalActions Int
  | GiveAdditionalAction AdditionalAction
  | AdditionalStartingUses Int
  | AdditionalCost Cost
  | ChangeRevealStrategy RevealStrategy
  | CannotTriggerAbilityMatching AbilityMatcher
  | ConnectedToWhen LocationMatcher LocationMatcher
  | AlternateSuccessfullEvasion
  | AlternateSuccessfullInvestigation
  | AlternativeReady Source
  | AnySkillValue Int
  | AsIfInHand Card
  | AsIfUnderControlOf InvestigatorId
  | AttacksCannotBeCancelled
  | BaseSkillOf { skillType :: SkillType, value :: Int }
  | BecomesFast
  | Blank
  | Blocked
  | CannotEnter LocationId
  | CanAssignDamageToAsset AssetId
  | CanAssignHorrorToAsset AssetId
  | CanBeAssignedDirectDamage
  | CanBeFoughtAsIfAtYourLocation
  | CanBecomeFast CardMatcher
  | CanBecomeFastOrReduceCostOf CardMatcher Int -- Used by Chuck Fergus (2), check for notes
  | RevealTokensBeforeCommittingCards
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
  | CancelAttacksByEnemies EnemyMatcher
  | CannotBeDamaged
  | CannotBeDamagedByPlayerSources SourceMatcher
  | CannotBeDamagedByPlayerSourcesExcept SourceMatcher
  | CannotBeEnteredByNonElite
  | CannotBeEvaded
  | CannotBeRevealed
  | CannotCancelHorror
  | CannotCancelOrIgnoreToken TokenFace
  | CannotCommitCards CardMatcher
  | CannotDiscoverClues
  | CannotDrawCards
  | CannotEngage InvestigatorId
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
  | CannotMulligan
  | CannotPerformSkillTest
  | CannotPlaceClues
  | CannotPlay CardMatcher
  | CannotPutIntoPlay CardMatcher
  | CannotSpendClues
  | MaxCluesDiscovered Int
  | CannotDiscoverCluesAt LocationMatcher
  | CannotTakeAction ActionTarget
  | MustTakeAction ActionTarget
  | CannotTakeControlOfClues
  | NoMoreThanOneDamageOrHorrorAmongst AssetMatcher
  | CannotTriggerFastAbilities
  | CardsCannotLeaveYourDiscardPile
  | ChangeTokenModifier TokenModifier
  | ControlledAssetsCannotReady
  | DamageDealt Int
  | DamageTaken Int
  | Difficulty Int
  | DiscoveredClues Int
  | DoNotRemoveDoom
  | DoNotTakeUpSlot SlotType
  | CannotPlaceDoomOnThis
  | DoNotDrawChaosTokensForSkillChecks
  | DoesNotDamageOtherInvestigator
  | DoomThresholdModifier Int
  | DoomSubtracts
  | DoubleDifficulty
  | DoubleNegativeModifiersOnTokens
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
  | ForcedTokenChange TokenFace [TokenFace]
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
  | IgnoreToken
  | IgnoreTokenEffects
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
  | SkillModifier { skillType :: SkillType, value ::  Int }
  | AddSkillValue SkillType
  | SkillCannotBeIncreased SkillType
  | SkipMythosPhaseStep MythosPhaseStep
  | SpawnNonEliteAtConnectingInstead
  | ForceSpawnLocation LocationMatcher
  | CannotSpawnIn LocationMatcher
  | StartingHand Int
  | StartingResources Int
  | StartingClues Int
  | TokenFaceModifier [TokenFace]
  | TokenValueModifier Int
  | TopCardOfDeckIsRevealed
  | TreatAllDamageAsDirect
  | TreatRevealedTokenAs TokenFace
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
  | RevealAnotherToken -- TODO: Only ShatteredAeons handles this, if a player card affects this, all scenarios have to be updated
  | IgnoreLimit
  | CanIgnoreLimit
  | DoNotExhaustEvaded
  | DoNotDisengageEvaded
  | CannotBeAdvancedByDoomThreshold
  | MetaModifier Value
  | CanModify ModifierType
  | NoSurge
  deriving stock (Show, Eq, Ord)

data Modifier = Modifier
  { modifierSource :: Source
  , modifierType :: ModifierType
  , modifierActiveDuringSetup :: Bool
  }
  deriving stock (Show, Eq, Ord)

data ActionTarget
  = FirstOneOf [Action]
  | IsAction Action
  | EnemyAction Action EnemyMatcher
  deriving stock (Show, Eq, Ord)

setActiveDuringSetup :: Modifier -> Modifier
setActiveDuringSetup m = m { modifierActiveDuringSetup = True }

$(deriveJSON defaultOptions ''ActionTarget)
$(do
    deriveModifierType <- deriveJSON defaultOptions ''ModifierType
    deriveModifier <- deriveJSON (aesonOptions $ Just "modifier") ''Modifier
    pure $ deriveModifierType ++ deriveModifier
  )
