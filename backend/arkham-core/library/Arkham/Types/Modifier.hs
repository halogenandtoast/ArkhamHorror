module Arkham.Types.Modifier
  ( Modifier(..)
  , ActionTarget(..)
  , isBlank
  )
where

import Arkham.Types.Action
import Arkham.Types.Card
import Arkham.Types.SkillType
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

data Modifier
  = ActionCostOf ActionTarget Int
  | ActionSkillModifier Action SkillType Int
  | AdditionalActions Int
  | AnySkillValue Int
  | BaseSkillOf SkillType Int
  | Blank
  | CanPlayTopOfDiscard (Maybe PlayerCardType, [Trait])
  | CannotBeAttackedByNonElite
  | CannotBeDiscarded
  | CannotBeEnteredByNonElite
  | CannotBeEvaded
  | CannotCancelHorror
  | CannotCommitCards
  | CannotDiscoverClues
  | CannotGainResources
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
  | ModifierIfSucceededBy Int Modifier
  | NegativeToPositive
  | ChangeTokenModifier TokenModifier
  | ReduceCostOf [Trait] Int
  | ReduceCostOfCardType PlayerCardType Int
  | SanityModifier Int
  | ShroudModifier Int
  | SkillModifier SkillType Int
  | SpawnNonEliteAtConnectingInstead
  | UseSkillInPlaceOf SkillType SkillType
  | XPModifier Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, Hashable)

data ActionTarget
  = FirstOneOf [Action]
  | IsAction Action
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, Hashable)

isBlank :: Modifier -> Bool
isBlank Blank{} = True
isBlank _ = False
