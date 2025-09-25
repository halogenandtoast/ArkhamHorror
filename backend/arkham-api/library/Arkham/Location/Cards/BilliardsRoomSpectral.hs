module Arkham.Location.Cards.BilliardsRoomSpectral (billiardsRoomSpectral) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.AtDeathsDoorstep.Helpers

newtype BilliardsRoomSpectral = BilliardsRoomSpectral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

billiardsRoomSpectral :: LocationCard BilliardsRoomSpectral
billiardsRoomSpectral = location BilliardsRoomSpectral Cards.billiardsRoomSpectral 3 (PerPlayer 1)

instance HasAbilities BilliardsRoomSpectral where
  getAbilities (BilliardsRoomSpectral a) =
    extendRevealed1 a $ scenarioI18n $ hauntedI "billiardsRoomSpectral.haunted" a 1

instance RunMessage BilliardsRoomSpectral where
  runMessage msg l@(BilliardsRoomSpectral attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ assetControlledBy iid <> DiscardableAsset
      chooseOrRunOneM iid $ withI18n do
        countVar 1 $ labeled' "takeDamage" $ assignDamage iid (attrs.ability 1) 1
        unless (null assets) do
          countVar 1 $ labeled' "discardsAssets" $ chooseAndDiscardAsset iid (attrs.ability 1)
      pure l
    _ -> BilliardsRoomSpectral <$> liftRunMessage msg attrs
