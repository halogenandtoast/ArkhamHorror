module Arkham.Location.Cards.ElMalecon (elMalecon) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ElMalecon = ElMalecon LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elMalecon :: LocationCard ElMalecon
elMalecon = symbolLabel $ location ElMalecon Cards.elMalecon 5 (PerPlayer 1)

instance HasAbilities ElMalecon where
  getAbilities (ElMalecon attrs) =
    extendRevealed attrs []

instance RunMessage ElMalecon where
  runMessage msg (ElMalecon attrs) = runQueueT $ case msg of
    _ -> ElMalecon <$> liftRunMessage msg attrs
