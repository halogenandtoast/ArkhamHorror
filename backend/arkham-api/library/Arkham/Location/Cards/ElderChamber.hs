module Arkham.Location.Cards.ElderChamber (elderChamber) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ElderChamber = ElderChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elderChamber :: LocationCard ElderChamber
elderChamber = location ElderChamber Cards.elderChamber 1 (PerPlayer 2)

instance HasAbilities ElderChamber where
  getAbilities (ElderChamber attrs) =
    extendRevealed attrs []

instance RunMessage ElderChamber where
  runMessage msg (ElderChamber attrs) = runQueueT $ case msg of
    _ -> ElderChamber <$> liftRunMessage msg attrs
