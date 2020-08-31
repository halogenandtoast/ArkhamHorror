module Arkham.Types.Modifier
  ( sourceOfModifier
  , replaceModifierSource
  , Modifier(..)
  , ActionTarget(..)
  )
where

import Arkham.Types.Action
import Arkham.Types.Card
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

sourceOfModifier :: Modifier -> Source
sourceOfModifier (ActionCostOf _ _ s) = s
sourceOfModifier (CannotPlay _ s) = s
sourceOfModifier (CannotInvestigate s) = s
sourceOfModifier (CannotDiscoverClues s) = s
sourceOfModifier (CannotSpendClues s) = s
sourceOfModifier (SkillModifier _ _ s) = s
sourceOfModifier (AnySkillValue _ s) = s
sourceOfModifier (SanityModifier _ s) = s
sourceOfModifier (HealthModifier _ s) = s
sourceOfModifier (ActionSkillModifier _ _ _ s) = s
sourceOfModifier (DamageTaken _ s) = s
sourceOfModifier (DamageDealt _ s) = s
sourceOfModifier (ShroudModifier _ s) = s
sourceOfModifier (DiscoveredClues _ s) = s
sourceOfModifier (UseSkillInPlaceOf _ _ s) = s
sourceOfModifier (ForcedTokenChange _ _ s) = s
sourceOfModifier (DoubleNegativeModifiersOnTokens s) = s
sourceOfModifier (ReduceCostOf _ _ s) = s
sourceOfModifier (EnemyFight _ s) = s
sourceOfModifier (EnemyEvade _ s) = s
sourceOfModifier (CannotBeAttackedByNonElite s) = s
sourceOfModifier (CannotBeEnteredByNonElite s) = s
sourceOfModifier (SpawnNonEliteAtConnectingInstead s) = s
sourceOfModifier (XPModifier _ s) = s
sourceOfModifier (Blank s) = s
sourceOfModifier (CanPlayTopOfDiscard _ s) = s
sourceOfModifier (AdditionalActions _ s) = s
sourceOfModifier (ModifierIfSucceededBy _ m) = sourceOfModifier m

replaceModifierSource :: Source -> Modifier -> Modifier
replaceModifierSource s (ActionCostOf a b _) = ActionCostOf a b s
replaceModifierSource s (CannotPlay a _) = CannotPlay a s
replaceModifierSource s (CannotInvestigate _) = CannotInvestigate s
replaceModifierSource s (CannotDiscoverClues _) = CannotDiscoverClues s
replaceModifierSource s (CannotSpendClues _) = CannotSpendClues s
replaceModifierSource s (SkillModifier a b _) = SkillModifier a b s
replaceModifierSource s (AnySkillValue a _) = AnySkillValue a s
replaceModifierSource s (SanityModifier a _) = SanityModifier a s
replaceModifierSource s (HealthModifier a _) = HealthModifier a s
replaceModifierSource s (ActionSkillModifier a b c _) =
  ActionSkillModifier a b c s
replaceModifierSource s (DamageTaken a _) = DamageTaken a s
replaceModifierSource s (DamageDealt a _) = DamageDealt a s
replaceModifierSource s (ShroudModifier a _) = ShroudModifier a s
replaceModifierSource s (DiscoveredClues a _) = DiscoveredClues a s
replaceModifierSource s (UseSkillInPlaceOf a b _) = UseSkillInPlaceOf a b s
replaceModifierSource s (ForcedTokenChange a b _) = ForcedTokenChange a b s
replaceModifierSource s (DoubleNegativeModifiersOnTokens _) =
  DoubleNegativeModifiersOnTokens s
replaceModifierSource s (ReduceCostOf a b _) = ReduceCostOf a b s
replaceModifierSource s (EnemyFight a _) = EnemyFight a s
replaceModifierSource s (EnemyEvade a _) = EnemyEvade a s
replaceModifierSource s (CannotBeAttackedByNonElite _) =
  CannotBeAttackedByNonElite s
replaceModifierSource s (CannotBeEnteredByNonElite _) =
  CannotBeEnteredByNonElite s
replaceModifierSource s (SpawnNonEliteAtConnectingInstead _) =
  SpawnNonEliteAtConnectingInstead s
replaceModifierSource s (XPModifier a _) = XPModifier a s
replaceModifierSource s (Blank _) = Blank s
replaceModifierSource s (CanPlayTopOfDiscard a _) = CanPlayTopOfDiscard a s
replaceModifierSource s (AdditionalActions a _) = AdditionalActions a s
replaceModifierSource s (ModifierIfSucceededBy a m) =
  ModifierIfSucceededBy a (replaceModifierSource s m)

data Modifier
  = ActionCostOf ActionTarget Int Source
  | ActionSkillModifier Action SkillType Int Source
  | AdditionalActions Int Source
  | Blank Source
  | CanPlayTopOfDiscard (Maybe PlayerCardType, [Trait]) Source
  | CannotBeAttackedByNonElite Source
  | CannotBeEnteredByNonElite Source
  | SpawnNonEliteAtConnectingInstead Source
  | CannotDiscoverClues Source
  | CannotInvestigate Source
  | CannotPlay [PlayerCardType] Source
  | CannotSpendClues Source
  | DamageDealt Int Source
  | DamageTaken Int Source
  | DiscoveredClues Int Source
  | DoubleNegativeModifiersOnTokens Source
  | EnemyEvade Int Source
  | EnemyFight Int Source
  | ForcedTokenChange Token Token Source
  | HealthModifier Int Source
  | ReduceCostOf [Trait] Int Source
  | SanityModifier Int Source
  | ShroudModifier Int Source
  | SkillModifier SkillType Int Source
  | AnySkillValue Int Source
  | UseSkillInPlaceOf SkillType SkillType Source
  | XPModifier Int Source
  | ModifierIfSucceededBy Int Modifier
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ActionTarget
  = FirstOneOf [Action]
  | IsAction Action
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
