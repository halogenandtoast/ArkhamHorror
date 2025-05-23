module Arkham.Location.Cards.EasttownArkhamPoliceStation (easttownArkhamPoliceStation) where

import Arkham.Ability
import Arkham.Asset.Uses
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (easttownArkhamPoliceStation)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype EasttownArkhamPoliceStation = EasttownArkhamPoliceStation LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

easttownArkhamPoliceStation :: LocationCard EasttownArkhamPoliceStation
easttownArkhamPoliceStation = location EasttownArkhamPoliceStation Cards.easttownArkhamPoliceStation 4 (PerPlayer 2)

instance HasAbilities EasttownArkhamPoliceStation where
  getAbilities (EasttownArkhamPoliceStation a) =
    extendRevealed1 a $ playerLimit PerGame $ restricted a 1 Here actionAbility

instance RunMessage EasttownArkhamPoliceStation where
  runMessage msg l@(EasttownArkhamPoliceStation attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      ammoAssets <- map (Ammo,) <$> select (assetControlledBy iid <> AssetWithUseType Ammo)
      supplyAssets <- map (Supply,) <$> select (assetControlledBy iid <> AssetWithUseType Supply)
      chooseOneM iid do
        for_ (ammoAssets <> supplyAssets) \(useType', asset) -> do
          targeting asset $ addUses (attrs.ability 1) asset useType' 2
      pure l
    _ -> EasttownArkhamPoliceStation <$> liftRunMessage msg attrs
