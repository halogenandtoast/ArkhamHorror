module Arkham.Location.Cards.CatacombsOfKomElShoqafaDenOfTheBeast (catacombsOfKomElShoqafaDenOfTheBeast) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.DogsOfWar.Helpers

newtype CatacombsOfKomElShoqafaDenOfTheBeast = CatacombsOfKomElShoqafaDenOfTheBeast LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

catacombsOfKomElShoqafaDenOfTheBeast :: LocationCard CatacombsOfKomElShoqafaDenOfTheBeast
catacombsOfKomElShoqafaDenOfTheBeast =
  symbolLabel
    $ location
      CatacombsOfKomElShoqafaDenOfTheBeast
      Cards.catacombsOfKomElShoqafaDenOfTheBeast
      5
      (PerPlayer 2)

instance HasModifiersFor CatacombsOfKomElShoqafaDenOfTheBeast where
  getModifiersFor (CatacombsOfKomElShoqafaDenOfTheBeast a) = do
    blockedWhen a $ runValidT do
      guard a.unrevealed
      locuses <- lift keyLocusLocations
      guard $ locuses /= [a.id]

instance HasAbilities CatacombsOfKomElShoqafaDenOfTheBeast where
  getAbilities (CatacombsOfKomElShoqafaDenOfTheBeast a) =
    extendRevealed a []

instance RunMessage CatacombsOfKomElShoqafaDenOfTheBeast where
  runMessage msg (CatacombsOfKomElShoqafaDenOfTheBeast attrs) = runQueueT $ case msg of
    _ -> CatacombsOfKomElShoqafaDenOfTheBeast <$> liftRunMessage msg attrs
