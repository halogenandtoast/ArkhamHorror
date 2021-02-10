module Arkham.Types.Effect.Effects.OnTheLam
  ( onTheLam
  , OnTheLam(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Helpers

newtype OnTheLam = OnTheLam EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheLam :: EffectArgs -> OnTheLam
onTheLam = OnTheLam . uncurry4 (baseAttrs "01010")

instance HasModifiersFor env OnTheLam where
  getModifiersFor _ target (OnTheLam a@EffectAttrs {..}) =
    pure $ toModifiers a [ CannotBeAttackedByNonElite | target == effectTarget ]

instance HasQueue env => RunMessage env OnTheLam where
  runMessage msg e@(OnTheLam attrs) = case msg of
    EndRound -> e <$ unshiftMessage (DisableEffect $ toId attrs)
    _ -> OnTheLam <$> runMessage msg attrs
