module Arkham.Types.Modifier
  ( Modifier(..)
  , ActionTarget(..)
  , replaceIntModifierValue
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
  | BaseSkillOf SkillType Int
  | Blank
  | CanPlayTopOfDiscard (Maybe PlayerCardType, [Trait])
  | CannotBeAttackedByNonElite
  | CannotBeEnteredByNonElite
  | SpawnNonEliteAtConnectingInstead
  | CannotDiscoverClues
  | CannotInvestigate
  | CannotPlay [PlayerCardType]
  | CannotSpendClues
  | ControlledAssetsCannotReady
  | DamageDealt Int
  | HorrorDealt Int
  | DamageTaken Int
  | DiscoveredClues Int
  | DoubleNegativeModifiersOnTokens
  | EnemyEvade Int
  | EnemyFight Int
  | ForcedTokenChange Token Token
  | HealthModifier Int
  | ReduceCostOf [Trait] Int
  | SanityModifier Int
  | ShroudModifier Int
  | SkillModifier SkillType Int
  | AnySkillValue Int
  | UseSkillInPlaceOf SkillType SkillType
  | XPModifier Int
  | ModifierIfSucceededBy Int Modifier
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ActionTarget
  = FirstOneOf [Action]
  | IsAction Action
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

replaceIntModifierValue :: Maybe (Int, Int) -> Int -> Modifier -> Modifier
replaceIntModifierValue mbounds n modifier = case modifier of
  ActionCostOf a _ -> ActionCostOf a val
  ActionSkillModifier a b _ -> ActionSkillModifier a b val
  AdditionalActions _ -> AdditionalActions val
  BaseSkillOf a _ -> BaseSkillOf a val
  Blank -> modifier
  CanPlayTopOfDiscard{} -> modifier
  CannotBeAttackedByNonElite -> modifier
  CannotBeEnteredByNonElite -> modifier
  SpawnNonEliteAtConnectingInstead -> modifier
  CannotDiscoverClues -> modifier
  CannotInvestigate -> modifier
  CannotPlay{} -> modifier
  CannotSpendClues -> modifier
  ControlledAssetsCannotReady -> modifier
  DamageDealt _ -> DamageDealt val
  HorrorDealt _ -> HorrorDealt val
  DamageTaken _ -> DamageTaken val
  DiscoveredClues _ -> DiscoveredClues val
  DoubleNegativeModifiersOnTokens -> modifier
  EnemyEvade _ -> EnemyEvade val
  EnemyFight _ -> EnemyFight val
  ForcedTokenChange{} -> modifier
  HealthModifier _ -> HealthModifier val
  ReduceCostOf a _ -> ReduceCostOf a val
  SanityModifier _ -> SanityModifier val
  ShroudModifier _ -> ShroudModifier val
  SkillModifier a _ -> SkillModifier a val
  AnySkillValue _ -> AnySkillValue val
  UseSkillInPlaceOf{} -> modifier
  XPModifier _ -> XPModifier val
  ModifierIfSucceededBy{} -> modifier
 where
  val = case mbounds of
    Nothing -> n
    Just (low, high) -> min low (max high n)

isBlank :: Modifier -> Bool
isBlank Blank{} = True
isBlank _ = False

