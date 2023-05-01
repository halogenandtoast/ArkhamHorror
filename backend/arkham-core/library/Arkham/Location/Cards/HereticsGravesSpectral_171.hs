module Arkham.Location.Cards.HereticsGravesSpectral_171 (
  hereticsGravesSpectral_171,
  HereticsGravesSpectral_171 (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype HereticsGravesSpectral_171 = HereticsGravesSpectral_171 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hereticsGravesSpectral_171 :: LocationCard HereticsGravesSpectral_171
hereticsGravesSpectral_171 = location HereticsGravesSpectral_171 Cards.hereticsGravesSpectral_171 7 (Static 0)

instance HasAbilities HereticsGravesSpectral_171 where
  getAbilities (HereticsGravesSpectral_171 attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage HereticsGravesSpectral_171 where
  runMessage msg (HereticsGravesSpectral_171 attrs) =
    HereticsGravesSpectral_171 <$> runMessage msg attrs
