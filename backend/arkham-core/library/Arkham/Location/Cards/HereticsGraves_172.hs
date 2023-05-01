module Arkham.Location.Cards.HereticsGraves_172 (
  hereticsGraves_172,
  HereticsGraves_172 (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype HereticsGraves_172 = HereticsGraves_172 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hereticsGraves_172 :: LocationCard HereticsGraves_172
hereticsGraves_172 = location HereticsGraves_172 Cards.hereticsGraves_172 4 (Static 0)

instance HasAbilities HereticsGraves_172 where
  getAbilities (HereticsGraves_172 attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage HereticsGraves_172 where
  runMessage msg (HereticsGraves_172 attrs) =
    HereticsGraves_172 <$> runMessage msg attrs
