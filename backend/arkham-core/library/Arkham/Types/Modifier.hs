module Arkham.Types.Modifier
  ( Modifier(..)
  , ModifierType(..)
  , ActionTarget(..)
  , isBlank
  )
where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Action
import Arkham.Types.Card
import Arkham.Types.Keyword
import Arkham.Types.SkillType
import Arkham.Types.Source
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
  | ActionSkillModifier Action SkillType Int
  | ActionsAreFree
  | AdditionalActions Int
  | AddKeyword Keyword
  | AnySkillValue Int
  | BaseSkillOf SkillType Int
  | Blank
  | Blocked
  | CanBeAssignedDamage
  | CanBecomeFast (Maybe PlayerCardType, [Trait])
  | CanPlayTopOfDiscard (Maybe PlayerCardType, [Trait])
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
  | CannotMove
  | CannotPlay [PlayerCardType]
  | CannotSpendClues
  | ControlledAssetsCannotReady
  | DamageDealt Int
  | DamageTaken Int
  | Difficulty Int
  | DiscoveredClues Int
  | DoNotDrawChaosTokensForSkillChecks
  | DoubleNegativeModifiersOnTokens
  | EnemyEvade Int
  | EnemyFight Int
  | ForcedTokenChange Token [Token]
  | HandSize Int
  | HealthModifier Int
  | HorrorDealt Int
  | IgnoreText
  | MaxDamageTaken Int
  | ModifierIfSucceededBy Int Modifier
  | NegativeToPositive
  | ChangeTokenModifier TokenModifier
  | ReduceCostOf [Trait] Int
  | ReduceCostOfCardType PlayerCardType Int
  | RemoveKeyword Keyword
  | SanityModifier Int
  | ShroudModifier Int
  | SkillModifier SkillType Int
  | TokenValueModifier Int
  | TreatAllDamageAsDirect
  | SpawnNonEliteAtConnectingInstead
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

isBlank :: Modifier -> Bool
isBlank (Modifier _ Blank{}) = True
isBlank _ = False
