module Arkham.Modifier (
  Modifier (..),
  ModifierType (..),
  ActionTarget (..),
) where

import Arkham.Prelude

import Arkham.Action
import Arkham.Card.CardCode
import Arkham.Card.CardType
import Arkham.ClassSymbol
import Arkham.Id
import Arkham.Json
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Phase
import Arkham.SkillType
import {-# SOURCE #-} Arkham.Source
import {-# SOURCE #-} Arkham.Target
import Arkham.Token
import Arkham.Trait

data Modifier = Modifier
  { modifierSource :: Source
  , modifierType :: ModifierType
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

instance ToJSON Modifier where
  toJSON = genericToJSON $ aesonOptions $ Just "modifier"
  toEncoding = genericToEncoding $ aesonOptions $ Just "modifier"

instance FromJSON Modifier where
  parseJSON = genericParseJSON $ aesonOptions $ Just "modifier"

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
  | CannotTriggerAbilityMatching AbilityMatcher
  | ConnectedToWhen LocationMatcher LocationMatcher
  | AlternateSuccessfullEvasion
  | AlternateSuccessfullInvestigation
  | AlternativeReady Source
  | AnySkillValue Int
  | AttacksCannotBeCancelled
  | BaseSkillOf SkillType Int
  | BecomesFast
  | Blank
  | Blocked
  | CanBeAssignedDamage
  | CanBeFoughtAsIfAtYourLocation
  | CanBecomeFast (Maybe CardType, [Trait])
  | CanCommitToSkillTestPerformedByAnInvestigatorAtAnotherLocation Int
  | CanOnlyBeAttackedByAbilityOn (HashSet CardCode)
  | CanOnlyUseCardsInRole ClassSymbol
  | CanPlayTopOfDiscard (Maybe CardType, [Trait])
  | CanPlayTopOfDeck CardMatcher
  | CanSpendResourcesOnCardFromInvestigator InvestigatorMatcher CardMatcher
  | CancelSkills
  | CannotAttack
  | CannotBeAttackedByNonElite
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
  | CannotPlay [(CardType, HashSet Trait)]
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
  | DoubleSuccess
  | DoubleBaseSkillValue
  | DuringEnemyPhaseMustMoveToward Target
  | EnemyCannotEngage InvestigatorId
  | EnemyEvade Int
  | EnemyFight Int
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
  | MaxDamageTaken Int
  | MayChooseNotToTakeUpkeepResources
  | ModifierIfSucceededBy Int Modifier
  | NegativeToPositive
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
  | SkillCannotBeIncreased SkillType
  | SkipMythosPhaseStep MythosPhaseStep
  | SpawnNonEliteAtConnectingInstead
  | StartingResources Int
  | TokenFaceModifier [TokenFace]
  | TokenValueModifier Int
  | TopCardOfDeckIsRevealed
  | TreatAllDamageAsDirect
  | UseSkillInPlaceOf SkillType SkillType
  | XPModifier Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, Hashable)

data ActionTarget
  = FirstOneOf [Action]
  | IsAction Action
  | EnemyAction Action [Trait]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, Hashable)
