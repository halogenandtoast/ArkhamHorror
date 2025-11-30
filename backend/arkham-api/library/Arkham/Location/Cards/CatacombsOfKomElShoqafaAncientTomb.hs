module Arkham.Location.Cards.CatacombsOfKomElShoqafaAncientTomb (catacombsOfKomElShoqafaAncientTomb) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CatacombsOfKomElShoqafaAncientTomb = CatacombsOfKomElShoqafaAncientTomb LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

catacombsOfKomElShoqafaAncientTomb :: LocationCard CatacombsOfKomElShoqafaAncientTomb
catacombsOfKomElShoqafaAncientTomb = symbolLabel $ location CatacombsOfKomElShoqafaAncientTomb Cards.catacombsOfKomElShoqafaAncientTomb 0 (Static 0)

instance HasAbilities CatacombsOfKomElShoqafaAncientTomb where
  getAbilities (CatacombsOfKomElShoqafaAncientTomb a) =
    extendRevealed a []

instance RunMessage CatacombsOfKomElShoqafaAncientTomb where
  runMessage msg (CatacombsOfKomElShoqafaAncientTomb attrs) = runQueueT $ case msg of
    _ -> CatacombsOfKomElShoqafaAncientTomb <$> liftRunMessage msg attrs
