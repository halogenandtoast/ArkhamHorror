module Arkham.Effect.Effects.CurseOfTheRougarouTabletToken
  ( curseOfTheRougarouTabletToken
  , CurseOfTheRougarouTabletToken(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Effect.Helpers
import Arkham.Message
import Arkham.Modifier

newtype CurseOfTheRougarouTabletToken = CurseOfTheRougarouTabletToken EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

curseOfTheRougarouTabletToken :: EffectArgs -> CurseOfTheRougarouTabletToken
curseOfTheRougarouTabletToken =
  CurseOfTheRougarouTabletToken . uncurry4 (baseAttrs "81001")

instance HasModifiersFor CurseOfTheRougarouTabletToken where
  getModifiersFor _ target (CurseOfTheRougarouTabletToken a@EffectAttrs {..})
    | target == effectTarget = pure [toModifier a CannotMove]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage CurseOfTheRougarouTabletToken where
  runMessage msg e@(CurseOfTheRougarouTabletToken attrs) = case msg of
    EndRound -> e <$ push (DisableEffect $ effectId attrs)
    _ -> CurseOfTheRougarouTabletToken <$> runMessage msg attrs
