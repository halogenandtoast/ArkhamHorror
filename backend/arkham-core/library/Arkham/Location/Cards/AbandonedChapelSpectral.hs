module Arkham.Location.Cards.AbandonedChapelSpectral
  ( abandonedChapelSpectral
  , AbandonedChapelSpectral(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype AbandonedChapelSpectral = AbandonedChapelSpectral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abandonedChapelSpectral :: LocationCard AbandonedChapelSpectral
abandonedChapelSpectral = location AbandonedChapelSpectral Cards.abandonedChapelSpectral 2 (Static 0)

instance HasAbilities AbandonedChapelSpectral where
  getAbilities (AbandonedChapelSpectral attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage AbandonedChapelSpectral where
  runMessage msg (AbandonedChapelSpectral attrs) =
    AbandonedChapelSpectral <$> runMessage msg attrs
