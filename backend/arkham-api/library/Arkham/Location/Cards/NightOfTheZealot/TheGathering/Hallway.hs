module Arkham.Location.Cards.NightOfTheZealot.TheGathering.Hallway (hallway) where

import Arkham.Location.CardDefs.NightOfTheZealot.TheGathering qualified as Cards
import Arkham.Location.Import.Lifted

newtype Hallway = Hallway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hallway :: LocationCard Hallway
hallway = location Hallway Cards.hallway 1 (Static 0)

instance RunMessage Hallway where
  runMessage msg (Hallway attrs) = Hallway <$> runMessage msg attrs
