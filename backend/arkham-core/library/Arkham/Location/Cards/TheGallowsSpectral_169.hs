module Arkham.Location.Cards.TheGallowsSpectral_169
  ( theGallowsSpectral_169
  , TheGallowsSpectral_169(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype TheGallowsSpectral_169 = TheGallowsSpectral_169 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGallowsSpectral_169 :: LocationCard TheGallowsSpectral_169
theGallowsSpectral_169 = location TheGallowsSpectral_169 Cards.theGallowsSpectral_169 3 (Static 0)

instance HasAbilities TheGallowsSpectral_169 where
  getAbilities (TheGallowsSpectral_169 attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage TheGallowsSpectral_169 where
  runMessage msg (TheGallowsSpectral_169 attrs) =
    TheGallowsSpectral_169 <$> runMessage msg attrs
