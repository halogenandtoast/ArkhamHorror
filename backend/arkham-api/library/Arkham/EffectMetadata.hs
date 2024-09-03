module Arkham.EffectMetadata (
  EffectMetadata (..),
  effectInt,
  effectMetaTarget,
) where

import Arkham.Ability.Types
import Arkham.Card.CardCode
import Arkham.Id
import Arkham.Modifier
import Arkham.Prelude
import Arkham.SkillType
import Arkham.Target

data EffectMetadata window a
  = EffectInt Int
  | EffectMessages [a]
  | EffectModifiers [Modifier]
  | EffectCardCodes [CardCode]
  | EffectMetaTarget Target
  | EffectMetaSkill SkillType
  | EffectAbility (Ability, [window])
  | EffectCost ActiveCostId
  | EffectText Text
  deriving stock (Eq, Show, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

effectInt :: Int -> Maybe (EffectMetadata window a)
effectInt = Just . EffectInt

effectMetaTarget :: Targetable target => target -> Maybe (EffectMetadata window a)
effectMetaTarget = Just . EffectMetaTarget . toTarget
