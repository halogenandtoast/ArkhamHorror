{-# LANGUAGE TemplateHaskell #-}
module Arkham.Modifier
  ( Modifier(..)
  , ModifierType(..)
  , ActionTarget(..)
  ) where

import Arkham.Prelude

import Arkham.Action
import {-# SOURCE #-} Arkham.Card
import Arkham.Card.CardCode
import Arkham.Card.CardType
import Arkham.ChaosBag.RevealStrategy
import Arkham.ClassSymbol
import {-# SOURCE #-} Arkham.Cost
import Arkham.Id
import Arkham.Json
import Arkham.Keyword
import {-# SOURCE #-} Arkham.Matcher
import Arkham.Phase
import Arkham.SkillType
import Arkham.SlotType
import {-# SOURCE #-} Arkham.Source
import {-# SOURCE #-} Arkham.Target
import Arkham.Token
import Arkham.Trait
import Data.Aeson.TH

data Modifier = Modifier
  { modifierSource :: Source
  , modifierType :: ModifierType
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Hashable

data ModifierType
  = ActionCostOf ActionTarget Int
  | TraitRestrictedModifier Trait ModifierType
  | ActionCostModifier Int
  | ActionCostSetToModifier Int
  | ActionSkillModifier Action SkillType Int
  | ActionsAreFree
  | AddKeyword Keyword
  | AddTrait Trait
  | AddSkillIcons [SkillType]
  | AdditionalActions Int
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
  | BaseSkillOf SkillType Int
  | BecomesFast
  | Blank
  | Blocked
  | CanBeAssignedDamage
  | CanBeAssignedDirectDamage
  | CanBeFoughtAsIfAtYourLocation
  | CanBecomeFast CardMatcher
  | CanCommitToSkillTestPerformedByAnInvestigatorAtAnotherLocation Int
  | CanOnlyBeAttackedByAbilityOn (HashSet CardCode)
  | CanOnlyUseCardsInRole ClassSymbol
  | CanPlayTopOfDiscard (Maybe CardType, [Trait])
  | CanPlayTopOfDeck CardMatcher
  | CanSpendResourcesOnCardFromInvestigator InvestigatorMatcher CardMatcher
  | CancelSkills
  | CannotAttack
  | CannotBeAttackedByNonElite
  | CannotBeFlipped
  | CannotBeDefeated
  | CanOnlyBeDefeatedBy Source
  | CanOnlyBeDefeatedByDamage
  | CancelAttacksByEnemies EnemyMatcher
  | CannotBeDamaged
  | CannotBeDamagedByPlayerSourcesExcept SourceMatcher
  | CannotBeDiscarded
  | CannotBeEnteredByNonElite
  | CannotBeEvaded
  | CannotBeRevealed
  | CannotCancelHorror
  | CannotCommitCards CardMatcher
  | CannotDiscoverClues
  | CannotDrawCards
  | CannotEngage InvestigatorId
  | CannotGainResources
  | CannotHealHorror
  | CannotInvestigate
  | CannotMakeAttacksOfOpportunity
  | CannotManipulateDeck
  | ActionDoesNotCauseAttacksOfOpportunity Action
  | CannotFight EnemyMatcher
  | CannotMove
  | CannotMoveMoreThanOnceEachTurn
  | CannotMulligan
  | CannotPerformSkillTest
  | CannotPlaceClues
  | CannotPlay CardMatcher
  | CannotSpendClues
  | CannotTakeAction ActionTarget
  | CannotTakeControlOfClues
  | CannotTriggerFastAbilities
  | CardsCannotLeaveYourDiscardPile
  | ChangeTokenModifier TokenModifier
  | ControlledAssetsCannotReady
  | DamageDealt Int
  | DamageTaken Int
  | Difficulty Int
  | DiscoveredClues Int
  | DoNotDrawChaosTokensForSkillChecks
  | DoesNotDamageOtherInvestigator
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
  | FewerSlots SlotType Int
  | ForcedTokenChange TokenFace [TokenFace]
  | HandSize Int
  | HandSizeCardCount Int
  | HealthModifier Int
  | HorrorDealt Int
  | HunterConnectedTo LocationId
  | IgnoreRetaliate
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
  | RemoveFromGameInsteadOfDiscard
  | RemoveKeyword Keyword
  | ReturnToHandAfterTest
  | SanityModifier Int
  | SetDifficulty Int
  | ShroudModifier Int
  | SkillModifier SkillType Int
  | AddSkillValue SkillType
  | SkillCannotBeIncreased SkillType
  | SkipMythosPhaseStep MythosPhaseStep
  | SpawnNonEliteAtConnectingInstead
  | SpawnLocation LocationMatcher
  | StartingResources Int
  | TokenFaceModifier [TokenFace]
  | TokenValueModifier Int
  | TopCardOfDeckIsRevealed
  | TreatAllDamageAsDirect
  | TreatRevealedTokenAs TokenFace
  | UseSkillInPlaceOf SkillType SkillType
  | XPModifier Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass Hashable

data ActionTarget
  = FirstOneOf [Action]
  | IsAction Action
  | EnemyAction Action [Trait]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, Hashable)

$(deriveJSON defaultOptions ''ModifierType)

instance ToJSON Modifier where
  toJSON = genericToJSON $ aesonOptions $ Just "modifier"
  toEncoding = genericToEncoding $ aesonOptions $ Just "modifier"

instance FromJSON Modifier where
  parseJSON = genericParseJSON $ aesonOptions $ Just "modifier"

