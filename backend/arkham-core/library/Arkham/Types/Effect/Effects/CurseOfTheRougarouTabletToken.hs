module Arkham.Types.Effect.Effects.CurseOfTheRougarouTabletToken
  ( curseOfTheRougarouTabletToken
  , CurseOfTheRougarouTabletToken(..)
  )
where


import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Helpers

newtype CurseOfTheRougarouTabletToken = CurseOfTheRougarouTabletToken EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

curseOfTheRougarouTabletToken :: EffectArgs -> CurseOfTheRougarouTabletToken
curseOfTheRougarouTabletToken =
  CurseOfTheRougarouTabletToken . uncurry4 (baseAttrs "81001")

instance HasModifiersFor env CurseOfTheRougarouTabletToken where
  getModifiersFor _ target (CurseOfTheRougarouTabletToken a@EffectAttrs {..})
    | target == effectTarget = pure [toModifier a CannotMove]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env CurseOfTheRougarouTabletToken where
  runMessage msg e@(CurseOfTheRougarouTabletToken attrs) = case msg of
    EndRound -> e <$ unshiftMessage (DisableEffect $ effectId attrs)
    _ -> CurseOfTheRougarouTabletToken <$> runMessage msg attrs
