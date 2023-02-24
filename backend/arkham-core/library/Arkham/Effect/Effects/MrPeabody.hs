module Arkham.Effect.Effects.MrPeabody
  ( MrPeabody(..)
  , mrPeabody
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Effect.Helpers
import Arkham.Message
import Arkham.Source
import Arkham.Trait

newtype MrPeabody = MrPeabody EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mrPeabody :: EffectArgs -> MrPeabody
mrPeabody = MrPeabody . uncurry4 (baseAttrs "03141")

instance HasModifiersFor MrPeabody where
  getModifiersFor target (MrPeabody attrs) | effectTarget attrs == target =
    pure $ toModifiers attrs [ShroudModifier (-1), AddTrait Passageway]
  getModifiersFor _ _ = pure []

instance RunMessage MrPeabody where
  runMessage msg e@(MrPeabody attrs) = case msg of
    Ready (AssetTarget aid) | AssetSource aid == effectSource attrs ->
      e <$ push (DisableEffect $ toId attrs)
    _ -> MrPeabody <$> runMessage msg attrs
