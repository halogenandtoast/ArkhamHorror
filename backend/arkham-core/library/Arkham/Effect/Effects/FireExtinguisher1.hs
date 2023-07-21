module Arkham.Effect.Effects.FireExtinguisher1 (
  fireExtinguisher1,
  FireExtinguisher1 (..),
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message

newtype FireExtinguisher1 = FireExtinguisher1 EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fireExtinguisher1 :: EffectArgs -> FireExtinguisher1
fireExtinguisher1 = FireExtinguisher1 . uncurry4 (baseAttrs "02114")

instance RunMessage FireExtinguisher1 where
  runMessage msg e@(FireExtinguisher1 attrs@EffectAttrs {..}) = case msg of
    PassedSkillTest iid (Just Action.Evade) _ (SkillTestInitiatorTarget (EnemyTarget _)) _ _
      | SkillTestTarget == effectTarget ->
          do
            evasions <-
              selectListMap (EnemyEvaded iid) $
                EnemyIsEngagedWith $
                  InvestigatorWithId iid
            e <$ pushAll (evasions <> [DisableEffect effectId])
    SkillTestEnds _ _ -> e <$ push (DisableEffect effectId)
    _ -> FireExtinguisher1 <$> runMessage msg attrs
