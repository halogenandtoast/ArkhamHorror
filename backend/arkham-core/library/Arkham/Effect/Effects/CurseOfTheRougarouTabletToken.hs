module Arkham.Effect.Effects.CurseOfTheRougarouTabletToken (
  curseOfTheRougarouTabletToken,
  CurseOfTheRougarouTabletToken (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Helpers
import Arkham.Effect.Runner

newtype CurseOfTheRougarouTabletToken = CurseOfTheRougarouTabletToken EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

curseOfTheRougarouTabletToken :: EffectArgs -> CurseOfTheRougarouTabletToken
curseOfTheRougarouTabletToken =
  CurseOfTheRougarouTabletToken . uncurry4 (baseAttrs "81001")

instance HasModifiersFor CurseOfTheRougarouTabletToken where
  getModifiersFor target (CurseOfTheRougarouTabletToken a@EffectAttrs {..})
    | target == effectTarget = pure [toModifier a CannotMove]
  getModifiersFor _ _ = pure []

instance RunMessage CurseOfTheRougarouTabletToken where
  runMessage msg e@(CurseOfTheRougarouTabletToken attrs) = case msg of
    EndRound -> e <$ push (DisableEffect $ effectId attrs)
    _ -> CurseOfTheRougarouTabletToken <$> runMessage msg attrs
