module Arkham.EffectMetadata (
  EffectMetadata (..),
  effectInt,
  effectAbility,
  effectMetaTarget,
) where

import Arkham.Ability.Types
import Arkham.Card.CardCode
import Arkham.Id
import Arkham.Modifier
import Arkham.Prelude
import Arkham.SkillType
import Arkham.Target

data EffectMetadata a
  = EffectInt Int
  | EffectMessages [a]
  | EffectModifiers [Modifier]
  | EffectCardCodes [CardCode]
  | EffectMetaTarget Target
  | EffectMetaSkill SkillType
  | EffectAbility Ability
  | EffectCost ActiveCostId
  | EffectText Text
  deriving stock (Eq, Show, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

effectAbility :: Ability -> Maybe (EffectMetadata a)
effectAbility = Just . EffectAbility

effectInt :: Int -> Maybe (EffectMetadata a)
effectInt = Just . EffectInt

effectMetaTarget :: Targetable target => target -> Maybe (EffectMetadata a)
effectMetaTarget = Just . EffectMetaTarget . toTarget
