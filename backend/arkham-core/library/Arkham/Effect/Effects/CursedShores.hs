module Arkham.Effect.Effects.CursedShores
  ( cursedShores
  , CursedShores(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Effect.Helpers
import Arkham.Message
import Arkham.Source
import Arkham.Target

newtype CursedShores = CursedShores EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cursedShores :: EffectArgs -> CursedShores
cursedShores = CursedShores . uncurry4 (baseAttrs "81007")

instance HasModifiersFor CursedShores where
  getModifiersFor SkillTestSource{} target (CursedShores a@EffectAttrs {..})
    | target == effectTarget = pure [toModifier a (AnySkillValue 2)]
  getModifiersFor _ _ _ = pure []

instance RunMessage CursedShores where
  runMessage msg e@(CursedShores attrs) = case msg of
    SkillTestEnds _ -> e <$ push (DisableEffect $ effectId attrs)
    EndTurn iid | InvestigatorTarget iid == effectTarget attrs ->
      e <$ push (DisableEffect $ effectId attrs)
    _ -> CursedShores <$> runMessage msg attrs
