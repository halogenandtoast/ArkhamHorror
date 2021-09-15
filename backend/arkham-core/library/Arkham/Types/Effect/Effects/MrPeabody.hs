module Arkham.Types.Effect.Effects.MrPeabody
  ( MrPeabody(..)
  , mrPeabody
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait

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
