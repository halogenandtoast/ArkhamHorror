module Arkham.Effect.Effects.FireExtinguisher1
  ( fireExtinguisher1
  , FireExtinguisher1(..)
  ) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Effect.Attrs
import Arkham.Id
import Arkham.Message
import Arkham.Target

newtype FireExtinguisher1 = FireExtinguisher1 EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fireExtinguisher1 :: EffectArgs -> FireExtinguisher1
fireExtinguisher1 = FireExtinguisher1 . uncurry4 (baseAttrs "02114")

instance HasModifiersFor env FireExtinguisher1

instance HasSet EnemyId env InvestigatorId => RunMessage env FireExtinguisher1 where
  runMessage msg e@(FireExtinguisher1 attrs@EffectAttrs {..}) = case msg of
    PassedSkillTest iid (Just Action.Evade) _ (SkillTestInitiatorTarget (EnemyTarget _)) _ _
      | SkillTestTarget == effectTarget
      -> do
        evasions <- map (EnemyEvaded iid) <$> getSetList @EnemyId iid
        e <$ pushAll (evasions <> [DisableEffect effectId])
    SkillTestEnds _ -> e <$ push (DisableEffect effectId)
    _ -> FireExtinguisher1 <$> runMessage msg attrs
