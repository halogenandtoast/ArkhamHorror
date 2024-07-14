module Arkham.Event.Cards.HiddenPocket (hiddenPocket, HiddenPocket (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Placement
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
      selectWithNonNull (assetControlledBy iid <> mapOneOf AssetWithTrait [Clothing, Armor])
        $ chooseOneToHandle iid attrs
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (AssetTarget aid) -> do
      push $ PlaceEvent iid attrs.id (AttachedToAsset aid Nothing)
      let addSlot sType =
            AddSlot iid sType
              $ AdjustableSlot (bothSource aid attrs) (Just $ CardWithTrait Illicit) [#hand, #accessory] []

      chooseOne
        iid
        [ Label "Start as hand slot" [addSlot #hand]
        , Label "Start as accessory slot" [addSlot #accessory]
        ]
      pure e
    _ -> HiddenPocket <$> liftRunMessage msg attrs
