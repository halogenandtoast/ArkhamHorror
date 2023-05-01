module Arkham.Location.Cards.TheGallowsSpectral_170
  ( theGallowsSpectral_170
  , TheGallowsSpectral_170(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype TheGallowsSpectral_170 = TheGallowsSpectral_170 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGallowsSpectral_170 :: LocationCard TheGallowsSpectral_170
theGallowsSpectral_170 = location TheGallowsSpectral_170 Cards.theGallowsSpectral_170 3 (Static 0)

instance HasAbilities TheGallowsSpectral_170 where
  getAbilities (TheGallowsSpectral_170 attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage TheGallowsSpectral_170 where
  runMessage msg (TheGallowsSpectral_170 attrs) =
    TheGallowsSpectral_170 <$> runMessage msg attrs
