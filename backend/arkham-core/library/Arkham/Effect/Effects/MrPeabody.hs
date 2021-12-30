module Arkham.Effect.Effects.MrPeabody
  ( MrPeabody(..)
  , mrPeabody
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Attrs
import Arkham.Effect.Helpers
import Arkham.Message
import Arkham.Modifier
import Arkham.Source
import Arkham.Target
import Arkham.Trait

newtype MrPeabody = MrPeabody EffectAttrs
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mrPeabody :: EffectArgs -> MrPeabody
mrPeabody = MrPeabody . uncurry4 (baseAttrs "03141")

instance HasModifiersFor env MrPeabody where
  getModifiersFor _ target (MrPeabody attrs) | effectTarget attrs == target =
    pure $ toModifiers attrs [ShroudModifier (-1), AddTrait Passageway]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env MrPeabody where
  runMessage msg e@(MrPeabody attrs) = case msg of
    Ready (AssetTarget aid) | AssetSource aid == effectSource attrs ->
      e <$ push (DisableEffect $ toId attrs)
    _ -> MrPeabody <$> runMessage msg attrs
