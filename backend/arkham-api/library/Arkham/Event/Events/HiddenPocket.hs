module Arkham.Event.Events.HiddenPocket (hiddenPocket) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Upgrade
import Arkham.Slot
import Arkham.Trait (Trait (Armor, Clothing, Illicit))

newtype HiddenPocket = HiddenPocket EventAttrs
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenPocket :: EventCard HiddenPocket
hiddenPocket = event HiddenPocket Cards.hiddenPocket

instance RunMessage HiddenPocket where
  runMessage msg e@(HiddenPocket attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      upgradeTargets <-
        getUpgradeTargets iid $ assetControlledBy iid <> mapOneOf AssetWithTrait [Clothing, Armor]
      chooseOneToHandle iid attrs upgradeTargets
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (AssetTarget aid) -> do
      place attrs $ AttachedToAsset aid Nothing
      let addSlot sType =
            AddSlot iid sType
              $ AdjustableSlot (bothSource aid attrs) (Just $ CardWithTrait Illicit) [#hand, #accessory] []

      chooseOneM iid do
        labeled "Start as hand slot" $ push $ addSlot #hand
        labeled "Start as accessory slot" $ push $ addSlot #accessory
      pure e
    _ -> HiddenPocket <$> liftRunMessage msg attrs
