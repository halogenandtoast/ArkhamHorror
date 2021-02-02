module Arkham.Types.Effect.Effects.CurseOfTheRougarouTabletToken
  ( curseOfTheRougarouTabletToken
  , CurseOfTheRougarouTabletToken(..)
  )
where

import Arkham.Import

import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Helpers

newtype CurseOfTheRougarouTabletToken = CurseOfTheRougarouTabletToken Attrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

curseOfTheRougarouTabletToken :: EffectArgs -> CurseOfTheRougarouTabletToken
curseOfTheRougarouTabletToken =
  CurseOfTheRougarouTabletToken . uncurry4 (baseAttrs "81001")

instance HasModifiersFor env CurseOfTheRougarouTabletToken where
  getModifiersFor _ target (CurseOfTheRougarouTabletToken a@Attrs {..})
    | target == effectTarget = pure [toModifier a CannotMove]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env CurseOfTheRougarouTabletToken where
  runMessage msg e@(CurseOfTheRougarouTabletToken attrs) = case msg of
    EndRound -> e <$ unshiftMessage (DisableEffect $ effectId attrs)
    _ -> CurseOfTheRougarouTabletToken <$> runMessage msg attrs
