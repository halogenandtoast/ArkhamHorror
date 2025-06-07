module Arkham.Location.Cards.MosaicChamber (mosaicChamber) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype MosaicChamber = MosaicChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mosaicChamber :: LocationCard MosaicChamber
mosaicChamber =
  location MosaicChamber Cards.mosaicChamber 3 (PerPlayer 1)
    & setConnectsTo (setFromList [LeftOf, RightOf])

instance HasAbilities MosaicChamber where
  getAbilities (MosaicChamber a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (Here <> HasSupply Canteen <> exists LocationWithAnyDoom) actionAbility

instance RunMessage MosaicChamber where
  runMessage msg l@(MosaicChamber attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseSelectM iid LocationWithAnyDoom \x -> removeDoom (attrs.ability 1) x 1
      pure l
    _ -> MosaicChamber <$> liftRunMessage msg attrs
