module Arkham.Location.Cards.Theatre (
  theatre,
  Theatre (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Theatre = Theatre LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

theatre :: LocationCard Theatre
theatre = location Theatre Cards.theatre 2 (Static 0)

instance RunMessage Theatre where
  runMessage msg (Theatre attrs) = Theatre <$> runMessage msg attrs
