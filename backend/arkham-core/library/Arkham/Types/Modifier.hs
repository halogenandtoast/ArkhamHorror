module Arkham.Types.Modifier
  ( Modifier(..)
  , ModifierType(..)
  , ActionTarget(..)
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Action
import Arkham.Types.Card
import Arkham.Types.ClassSymbol
import Arkham.Types.Id
import Arkham.Types.Keyword
import Arkham.Types.Matcher
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait

data Modifier = Modifier
  { modifierSource :: Source
  , modifierType :: ModifierType
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Hashable

instance ToJSON Modifier where
  toJSON = genericToJSON $ aesonOptions $ Just "modifier"
  toEncoding = genericToEncoding $ aesonOptions $ Just "modifier"

instance FromJSON Modifier where
  parseJSON = genericParseJSON $ aesonOptions $ Just "modifier"

data ModifierType
  = ActionCostOf ActionTarget Int
  | ActionCostModifier Int
  | ActionCostSetToModifier Int
  | ActionSkillModifier Action SkillType Int
  | ActionsAreFree
  | AdditionalActions Int
  | AdditionalStartingUses Int
  | AddKeyword Keyword
  | AlternateSuccessfullInvestigation
  | AlternateSuccessfullEvasion
  | AlternativeReady Source
  | AnySkillValue Int
  | BaseSkillOf SkillType Int
  | BecomesFast
  | Blank
  | Blocked
  | CancelSkills
  | CanBeAssignedDamage
  | CanBeFoughtAsIfAtYourLocation
  | CanBecomeFast (Maybe CardType, [Trait])
  | CanCommitToSkillTestPerformedByAnInvestigatorAtAnotherLocation Int
  | CanOnlyUseCardsInRole ClassSymbol
  | CanPlayTopOfDiscard (Maybe CardType, [Trait])
  | PlaceOnBottomOfDeckInsteadOfDiscard
  | CannotBeAttackedByNonElite
  | CannotBeDiscarded
  | CannotBeEnteredByNonElite
  | CannotBeEvaded
  | CannotBeRevealed
  | CannotCancelHorror
  | CannotCommitCards
  | CannotDiscoverClues
  | CannotDrawCards
  | CannotGainResources
  | CannotTakeAction ActionTarget
  | CannotHealHorror
  | CannotInvestigate
  | CannotMakeAttacksOfOpportunity
  | CannotMove
  | CannotMulligan
  | CannotPerformSkillTest
  | CannotPlaceClues
  | CannotPlay [(CardType, HashSet Trait)]
  | CannotSpendClues
  | CanOnlyBeAttackedByAbilityOn (HashSet CardCode)
  | CardsCannotLeaveYourDiscardPile
  | ControlledAssetsCannotReady
  | DamageDealt Int
  | DamageTaken Int
  | Difficulty Int
  | SetDifficulty Int
  | DiscoveredClues Int
  | DoNotDrawChaosTokensForSkillChecks
  | DoesNotDamageOtherInvestigator
  | DoubleNegativeModifiersOnTokens
  | DoubleDifficulty
  | DoubleSuccess
  | DuringEnemyPhaseMustMoveToward Target
  | EnemyCannotEngage InvestigatorId
  | EnemyEvade Int
  | EnemyFight Int
  | ForcedTokenChange TokenFace [TokenFace]
  | HandSize Int
  | HealthModifier Int
  | HorrorDealt Int
  | HunterConnectedTo LocationId
  | IgnoreText
  | IgnoreToken
  | IgnoreTokenEffects
  | MaxDamageTaken Int
  | MayChooseNotToTakeUpkeepResources
  | ModifierIfSucceededBy Int Modifier
  | NegativeToPositive
  | ChangeTokenModifier TokenModifier
  | ReduceCostOf CardMatcher Int
  | RemoveKeyword Keyword
  | RemoveFromGameInsteadOfDiscard
  | ReturnToHandAfterTest
  | SanityModifier Int
  | ShroudModifier Int
  | SkillModifier SkillType Int
  | StartingResources Int
  | TokenValueModifier Int
  | TokenFaceModifier [TokenFace]
  | TreatAllDamageAsDirect
  | SpawnNonEliteAtConnectingInstead
  | UseSkillInPlaceOf SkillType SkillType
  | AddSkillIcons [SkillType]
  | XPModifier Int
  | CannotEngage InvestigatorId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, Hashable)

data ActionTarget
  = FirstOneOf [Action]
  | IsAction Action
  | EnemyAction Action [Trait]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, Hashable)
