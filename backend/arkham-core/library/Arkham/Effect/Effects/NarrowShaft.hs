module Arkham.Effect.Effects.NarrowShaft
  ( narrowShaft
  , NarrowShaft(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Game.Helpers
import Arkham.Message

newtype NarrowShaft = NarrowShaft EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

narrowShaft :: EffectArgs -> NarrowShaft
narrowShaft =
  NarrowShaft . uncurry4 (baseAttrs "03254")

instance HasModifiersFor NarrowShaft

instance RunMessage NarrowShaft where
  runMessage msg e@(NarrowShaft attrs) = case msg of
    PassedSkillTest _ _ (LocationSource lid) SkillTestInitiatorTarget{} _ _ ->
      do
        narrowShaftId <- getJustLocationIdByName "Narrow Shaft"
        let disable = DisableEffect (effectId attrs)
        e <$ when
          (lid == narrowShaftId)
          (case effectMetadata attrs of
            Just (EffectMessages msgs) -> pushAll (msgs <> [disable])
            _ -> push disable
          )
    FailedSkillTest iid _ (LocationSource lid) SkillTestInitiatorTarget{} _ _ ->
      do
        narrowShaftId <- getJustLocationIdByName "Narrow Shaft"
        e <$ when
          (lid == narrowShaftId)
          (pushAll [InvestigatorAssignDamage iid (LocationSource narrowShaftId) DamageAny 1 0 , DisableEffect $ effectId attrs])
    _ -> NarrowShaft <$> runMessage msg attrs
