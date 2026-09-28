module Arkham.Homebrew.CircusExMortis.Locations.BanquetHall (banquetHall) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype BanquetHall = BanquetHall LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

banquetHall :: LocationCard BanquetHall
banquetHall = location BanquetHall Cards.banquetHall 4 (PerPlayer 1)

instance HasModifiersFor BanquetHall where
  getModifiersFor (BanquetHall a) = viceShroudReduction a Revelry

instance HasAbilities BanquetHall where
  getAbilities (BanquetHall a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ FastAbility
      $ OrCost [UseCost (AssetControlledBy You) uType 1 | uType <- [#supply, #ammo, #charge, #secret]]

instance RunMessage BanquetHall where
  runMessage msg l@(BanquetHall attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      parleyBonusAt (attrs.ability 1) iid attrs.id
      pure l
    _ -> BanquetHall <$> liftRunMessage msg attrs
