module Arkham.Effect.Effects.ExposeWeakness1 (
  exposeWeakness1,
  ExposeWeakness1 (..),
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Game.Helpers

newtype ExposeWeakness1 = ExposeWeakness1 EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exposeWeakness1 :: EffectArgs -> ExposeWeakness1
exposeWeakness1 = ExposeWeakness1 . uncurry4 (baseAttrs "02228")

instance HasModifiersFor ExposeWeakness1 where
  getModifiersFor target (ExposeWeakness1 attrs)
    | target == effectTarget attrs = case effectMetadata attrs of
        Just (EffectInt n) -> pure $ toModifiers attrs [EnemyFight (-n)]
        _ -> error "invalid effect metadata"
  getModifiersFor _ _ = pure []

instance RunMessage ExposeWeakness1 where
  runMessage msg e@(ExposeWeakness1 attrs@EffectAttrs {..}) = case msg of
    PassedSkillTest _ (Just Action.Fight) _ (SkillTestInitiatorTarget target) _ _
      | target == effectTarget ->
          e <$ push (DisableEffect effectId)
    FailedSkillTest _ (Just Action.Fight) _ (SkillTestInitiatorTarget target) _ _
      | target == effectTarget ->
          e <$ push (DisableEffect effectId)
    EndPhase -> e <$ push (DisableEffect effectId)
    _ -> ExposeWeakness1 <$> runMessage msg attrs
