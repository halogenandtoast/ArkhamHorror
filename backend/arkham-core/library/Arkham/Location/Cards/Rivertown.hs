module Arkham.Location.Cards.Rivertown where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (rivertown)
import Arkham.Location.Runner

newtype Rivertown = Rivertown LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

rivertown :: LocationCard Rivertown
rivertown = location Rivertown Cards.rivertown 1 (PerPlayer 1)

instance RunMessage Rivertown where
  runMessage msg (Rivertown attrs) = Rivertown <$> runMessage msg attrs
