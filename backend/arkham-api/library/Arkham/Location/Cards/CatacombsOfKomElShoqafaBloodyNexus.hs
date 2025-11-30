module Arkham.Location.Cards.CatacombsOfKomElShoqafaBloodyNexus (catacombsOfKomElShoqafaBloodyNexus) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CatacombsOfKomElShoqafaBloodyNexus = CatacombsOfKomElShoqafaBloodyNexus LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

catacombsOfKomElShoqafaBloodyNexus :: LocationCard CatacombsOfKomElShoqafaBloodyNexus
catacombsOfKomElShoqafaBloodyNexus = symbolLabel $ location CatacombsOfKomElShoqafaBloodyNexus Cards.catacombsOfKomElShoqafaBloodyNexus 0 (Static 0)

instance HasAbilities CatacombsOfKomElShoqafaBloodyNexus where
  getAbilities (CatacombsOfKomElShoqafaBloodyNexus a) =
    extendRevealed a []

instance RunMessage CatacombsOfKomElShoqafaBloodyNexus where
  runMessage msg (CatacombsOfKomElShoqafaBloodyNexus attrs) = runQueueT $ case msg of
    _ -> CatacombsOfKomElShoqafaBloodyNexus <$> liftRunMessage msg attrs
