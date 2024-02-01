module Arkham.Effect.Effects.NarrowShaft (
  narrowShaft,
  NarrowShaft (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner

newtype NarrowShaft = NarrowShaft EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

narrowShaft :: EffectArgs -> NarrowShaft
narrowShaft = NarrowShaft . uncurry4 (baseAttrs "03254")

instance RunMessage NarrowShaft where
  runMessage msg e@(NarrowShaft attrs) = case msg of
    PassedThisSkillTest _ (LocationSource lid) -> do
      narrowShaftId <- getJustLocationByName "Narrow Shaft"
      when (lid == narrowShaftId)
        $ case effectMetadata attrs of
          Just (EffectMessages msgs) -> pushAll (msgs <> [disable attrs])
          _ -> push $ disable attrs
      pure e
    FailedThisSkillTest iid (LocationSource lid) -> do
      narrowShaftId <- getJustLocationByName "Narrow Shaft"
      when (lid == narrowShaftId)
        $ pushAll [assignDamage iid narrowShaftId 1, disable attrs]
      pure e
    _ -> NarrowShaft <$> runMessage msg attrs
