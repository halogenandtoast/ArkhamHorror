module Arkham.Location.Cards.IdolChamber (
  idolChamber,
  IdolChamber (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted

newtype IdolChamber = IdolChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

idolChamber :: LocationCard IdolChamber
idolChamber = locationWith IdolChamber Cards.idolChamber 4 (PerPlayer 1) connectsToAdjacent

instance HasAbilities IdolChamber where
  getAbilities (IdolChamber attrs) =
    extendRevealed attrs []

instance RunMessage IdolChamber where
  runMessage msg (IdolChamber attrs) = runQueueT $ case msg of
    _ -> IdolChamber <$> liftRunMessage msg attrs
