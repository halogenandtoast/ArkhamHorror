module Arkham.Effect.Effects.Overzealous
  ( Overzealous(..)
  , overzealous
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Game.Helpers
import Arkham.Id
import Arkham.Keyword qualified as Keyword
import Arkham.Message
import Arkham.Target

newtype Overzealous = Overzealous EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overzealous :: EffectArgs -> Overzealous
overzealous = Overzealous . uncurry4 (baseAttrs "03040")

instance HasModifiersFor Overzealous where
  getModifiersFor target (Overzealous a) | effectTarget a == target =
    pure $ toModifiers a [AddKeyword Keyword.Surge]
  getModifiersFor _ _ = pure []

instance RunMessage Overzealous where
  runMessage msg e@(Overzealous attrs) = case msg of
    Discard _ (TreacheryTarget tid)
      | effectTarget attrs == CardIdTarget (unTreacheryId tid) -> e
      <$ push (DisableEffect $ effectId attrs)
    _ -> Overzealous <$> runMessage msg attrs
