module Arkham.Location.Cards.Rivertown (rivertown) where

import Arkham.Location.Cards qualified as Cards (rivertown)
import Arkham.Location.Import.Lifted

newtype Rivertown = Rivertown LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

rivertown :: LocationCard Rivertown
rivertown = location Rivertown Cards.rivertown 1 (PerPlayer 1)

instance RunMessage Rivertown where
  runMessage msg (Rivertown attrs) = Rivertown <$> runMessage msg attrs
