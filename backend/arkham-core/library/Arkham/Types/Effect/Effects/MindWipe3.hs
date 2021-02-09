module Arkham.Types.Effect.Effects.MindWipe3
  ( mindWipe3
  , MindWipe3(..)
  )
where


import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Helpers

newtype MindWipe3 = MindWipe3 EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mindWipe3 :: EffectArgs -> MindWipe3
mindWipe3 = MindWipe3 . uncurry4 (baseAttrs "50008")

instance HasModifiersFor env MindWipe3 where
  getModifiersFor _ target (MindWipe3 a@EffectAttrs {..}) | target == effectTarget =
    pure $ toModifiers a [Blank, DamageDealt (-1), HorrorDealt (-1)]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env MindWipe3 where
  runMessage msg e@(MindWipe3 attrs) = case msg of
    EndPhase -> e <$ unshiftMessage (DisableEffect $ effectId attrs)
    _ -> MindWipe3 <$> runMessage msg attrs
