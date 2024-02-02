module Arkham.Effect.Effects.MindWipe3 (
  mindWipe3,
  MindWipe3 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Helpers
import Arkham.Effect.Runner

newtype MindWipe3 = MindWipe3 EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

mindWipe3 :: EffectArgs -> MindWipe3
mindWipe3 = MindWipe3 . uncurry4 (baseAttrs "50008")

instance HasModifiersFor MindWipe3 where
  getModifiersFor target (MindWipe3 a@EffectAttrs {..})
    | target == effectTarget =
        pure
          $ toModifiers a [Blank, DamageDealt (-1), HorrorDealt (-1)]
  getModifiersFor _ _ = pure []

instance RunMessage MindWipe3 where
  runMessage msg e@(MindWipe3 attrs) = case msg of
    EndPhase -> e <$ push (DisableEffect $ effectId attrs)
    _ -> MindWipe3 <$> runMessage msg attrs
