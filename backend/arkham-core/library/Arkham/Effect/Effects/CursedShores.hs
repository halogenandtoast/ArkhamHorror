module Arkham.Effect.Effects.CursedShores (
  cursedShores,
  CursedShores (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Helpers
import Arkham.Effect.Runner

newtype CursedShores = CursedShores EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

cursedShores :: EffectArgs -> CursedShores
cursedShores = CursedShores . uncurry4 (baseAttrs "81007")

instance HasModifiersFor CursedShores where
  getModifiersFor target (CursedShores a@EffectAttrs {..})
    | target == effectTarget = do
        mSkillTestSource <- getSkillTestSource
        pure [toModifier a (AnySkillValue 2) | isJust mSkillTestSource]
  getModifiersFor _ _ = pure []

instance RunMessage CursedShores where
  runMessage msg e@(CursedShores attrs) = case msg of
    SkillTestEnds _ _ -> e <$ push (DisableEffect $ effectId attrs)
    EndTurn iid
      | InvestigatorTarget iid == effectTarget attrs ->
          e <$ push (DisableEffect $ effectId attrs)
    _ -> CursedShores <$> runMessage msg attrs
