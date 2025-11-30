module Arkham.Location.Cards.CatacombsOfKomElShoqafaDenOfTheBeast (catacombsOfKomElShoqafaDenOfTheBeast) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CatacombsOfKomElShoqafaDenOfTheBeast = CatacombsOfKomElShoqafaDenOfTheBeast LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

catacombsOfKomElShoqafaDenOfTheBeast :: LocationCard CatacombsOfKomElShoqafaDenOfTheBeast
catacombsOfKomElShoqafaDenOfTheBeast = symbolLabel $ location CatacombsOfKomElShoqafaDenOfTheBeast Cards.catacombsOfKomElShoqafaDenOfTheBeast 0 (Static 0)

instance HasAbilities CatacombsOfKomElShoqafaDenOfTheBeast where
  getAbilities (CatacombsOfKomElShoqafaDenOfTheBeast a) =
    extendRevealed a []

instance RunMessage CatacombsOfKomElShoqafaDenOfTheBeast where
  runMessage msg (CatacombsOfKomElShoqafaDenOfTheBeast attrs) = runQueueT $ case msg of
    _ -> CatacombsOfKomElShoqafaDenOfTheBeast <$> liftRunMessage msg attrs
