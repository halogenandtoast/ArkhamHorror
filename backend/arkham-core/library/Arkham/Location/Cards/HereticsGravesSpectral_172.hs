module Arkham.Location.Cards.HereticsGravesSpectral_172 (
  hereticsGravesSpectral_172,
  HereticsGravesSpectral_172 (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype HereticsGravesSpectral_172 = HereticsGravesSpectral_172 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hereticsGravesSpectral_172 :: LocationCard HereticsGravesSpectral_172
hereticsGravesSpectral_172 = location HereticsGravesSpectral_172 Cards.hereticsGravesSpectral_172 4 (Static 0)

instance HasAbilities HereticsGravesSpectral_172 where
  getAbilities (HereticsGravesSpectral_172 attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage HereticsGravesSpectral_172 where
  runMessage msg (HereticsGravesSpectral_172 attrs) =
    HereticsGravesSpectral_172 <$> runMessage msg attrs
