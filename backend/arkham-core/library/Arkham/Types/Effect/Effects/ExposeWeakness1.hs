module Arkham.Types.Effect.Effects.ExposeWeakness1
  ( exposeWeakness1
  , ExposeWeakness1(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.EffectMetadata
import Arkham.Types.Game.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype ExposeWeakness1 = ExposeWeakness1 EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exposeWeakness1 :: EffectArgs -> ExposeWeakness1
exposeWeakness1 = ExposeWeakness1 . uncurry4 (baseAttrs "02228")

instance HasModifiersFor env ExposeWeakness1 where
  getModifiersFor _ target (ExposeWeakness1 attrs)
    | target == effectTarget attrs = case effectMetadata attrs of
      Just (EffectInt n) -> pure $ toModifiers attrs [EnemyFight (-n)]
      _ -> error "invalid effect metadata"
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env ExposeWeakness1 where
  runMessage msg e@(ExposeWeakness1 attrs@EffectAttrs {..}) = case msg of
    PassedSkillTest _ (Just Action.Fight) _ (SkillTestInitiatorTarget target) _ _
      | target == effectTarget
      -> e <$ push (DisableEffect effectId)
    FailedSkillTest _ (Just Action.Fight) _ (SkillTestInitiatorTarget target) _ _
      | target == effectTarget
      -> e <$ push (DisableEffect effectId)
    EndPhase -> e <$ push (DisableEffect effectId)
    _ -> ExposeWeakness1 <$> runMessage msg attrs
