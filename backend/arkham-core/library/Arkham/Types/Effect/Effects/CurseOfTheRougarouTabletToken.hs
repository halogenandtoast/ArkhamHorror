module Arkham.Types.Effect.Effects.CurseOfTheRougarouTabletToken
  ( curseOfTheRougarouTabletToken
  , CurseOfTheRougarouTabletToken(..)
  )
where

import Arkham.Import

import Arkham.Types.Effect.Attrs

newtype CurseOfTheRougarouTabletToken = CurseOfTheRougarouTabletToken Attrs
  deriving newtype (Show, ToJSON, FromJSON)

curseOfTheRougarouTabletToken :: EffectArgs -> CurseOfTheRougarouTabletToken
curseOfTheRougarouTabletToken =
  CurseOfTheRougarouTabletToken . uncurry4 (baseAttrs "81001")

instance HasModifiersFor env CurseOfTheRougarouTabletToken where
  getModifiersFor _ target (CurseOfTheRougarouTabletToken Attrs {..})
    | target == effectTarget = pure [CannotMove]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env CurseOfTheRougarouTabletToken where
  runMessage msg e@(CurseOfTheRougarouTabletToken attrs) = case msg of
    EndRound -> e <$ unshiftMessage (DisableEffect $ effectId attrs)
    _ -> CurseOfTheRougarouTabletToken <$> runMessage msg attrs
