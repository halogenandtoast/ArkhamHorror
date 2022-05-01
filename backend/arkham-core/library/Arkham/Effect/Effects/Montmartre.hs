module Arkham.Effect.Effects.Montmartre
  ( Montmartre(..)
  , montmartre
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Attrs
import Arkham.Game.Helpers
import Arkham.Target
import Arkham.Modifier

newtype Montmartre = Montmartre EffectAttrs
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

montmartre :: EffectArgs -> Montmartre
montmartre = Montmartre . uncurry4 (baseAttrs "03209")

instance HasModifiersFor env Montmartre where
  getModifiersFor _ (InvestigatorTarget _) (Montmartre a) =
    pure $ toModifiers a [TopCardOfDeckIsRevealed]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env Montmartre where
  runMessage msg (Montmartre attrs) =
    Montmartre <$> runMessage msg attrs
