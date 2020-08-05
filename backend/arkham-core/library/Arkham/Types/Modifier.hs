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
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Token
import ClassyPrelude
import Data.Aeson

sourceOfModifier :: Modifier -> Source
sourceOfModifier (ActionCostOf _ _ s) = s
sourceOfModifier (CannotPlay _ s) = s
sourceOfModifier (SkillModifier _ _ s) = s
sourceOfModifier (ActionSkillModifier _ _ _ s) = s
sourceOfModifier (DamageTaken _ s) = s
sourceOfModifier (DamageDealt _ s) = s
sourceOfModifier (ShroudModifier _ s) = s
sourceOfModifier (DiscoveredClues _ s) = s
sourceOfModifier (SufferTrauma _ _ s) = s
sourceOfModifier (AddSlot _ _ s) = s
sourceOfModifier (UseSkillInPlaceOf _ _ s) = s
sourceOfModifier (ForcedTokenChange _ _ s) = s

replaceModifierSource :: Source -> Modifier -> Modifier
replaceModifierSource s (ActionCostOf a b _) = ActionCostOf a b s
replaceModifierSource s (CannotPlay a _) = CannotPlay a s
replaceModifierSource s (SkillModifier a b _) = SkillModifier a b s
replaceModifierSource s (ActionSkillModifier a b c _) =
  ActionSkillModifier a b c s
replaceModifierSource s (DamageTaken a _) = DamageTaken a s
replaceModifierSource s (DamageDealt a _) = DamageDealt a s
replaceModifierSource s (ShroudModifier a _) = ShroudModifier a s
replaceModifierSource s (DiscoveredClues a _) = DiscoveredClues a s
replaceModifierSource s (SufferTrauma a b _) = SufferTrauma a b s
replaceModifierSource s (AddSlot a b _) = AddSlot a b s
replaceModifierSource s (UseSkillInPlaceOf a b _) = UseSkillInPlaceOf a b s
replaceModifierSource s (ForcedTokenChange a b _) = ForcedTokenChange a b s

data Modifier
  = ActionCostOf ActionTarget Int Source
  | CannotPlay [PlayerCardType] Source
  | SkillModifier SkillType Int Source
  | ActionSkillModifier Action SkillType Int Source
  | AddSlot SlotType Slot Source
  | DamageDealt Int Source
  | DamageTaken Int Source
  | ShroudModifier Int Source
  | DiscoveredClues Int Source
  | SufferTrauma Int Int Source
  | UseSkillInPlaceOf SkillType SkillType Source
  | ForcedTokenChange Token Token Source
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ActionTarget
  = FirstOneOf [Action]
  | IsAction Action
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
