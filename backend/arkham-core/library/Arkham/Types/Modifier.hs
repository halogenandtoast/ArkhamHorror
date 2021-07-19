module Arkham.Types.Modifier
  ( Modifier(..)
  , ModifierType(..)
  , ActionTarget(..)
  , isBlank
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Action
import Arkham.Types.Card
import Arkham.Types.Id
import Arkham.Types.Keyword
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
  | AddKeyword Keyword
  | AlternateSuccessfullInvestigation
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
  | CanPlayTopOfDiscard (Maybe CardType, [Trait])
  | CannotBeAttackedByNonElite
  | CannotBeDiscarded
  | CannotBeEnteredByNonElite
  | CannotBeEvaded
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
  | CannotPlaceClues
  | CannotPlay [(CardType, HashSet Trait)]
  | CannotSpendClues
  | CanOnlyBeAttackedByAbilityOn (HashSet CardCode)
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
  | EnemyEvade Int
  | EnemyFight Int
  | ForcedTokenChange TokenFace [TokenFace]
  | HandSize Int
  | HealthModifier Int
  | HorrorDealt Int
  | HunterConnectedTo LocationId
  | IgnoreText
  | IgnoreTokenEffects
  | MaxDamageTaken Int
  | MayChooseNotToTakeUpkeepResources
  | ModifierIfSucceededBy Int Modifier
  | NegativeToPositive
  | ChangeTokenModifier TokenModifier
  | ReduceCostOf [Trait] Int
  | ReduceCostOfCardType CardType Int
  | RemoveKeyword Keyword
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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, Hashable)

data ActionTarget
  = FirstOneOf [Action]
  | IsAction Action
  | EnemyAction Action [Trait]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, Hashable)

isBlank :: Modifier -> Bool
isBlank (Modifier _ Blank{}) = True
isBlank _ = False
