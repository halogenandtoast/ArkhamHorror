module Arkham.Event.Events.EmergencyCache3 (emergencyCache3, EmergencyCache3 (..)) where

import Arkham.Asset.Uses
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype EmergencyCache3 = EmergencyCache3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emergencyCache3 :: EventCard EmergencyCache3
emergencyCache3 = event EmergencyCache3 Cards.emergencyCache3

instance RunMessage EmergencyCache3 where
  runMessage msg e@(EmergencyCache3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      supplyAssets <-
        select
          $ AssetControlledBy (affectsOthers $ colocatedWith iid)
          <> AssetCanHaveUses Supply
          <> AssetNotAtUseLimit
      if null supplyAssets
        then pushAll [TakeResources iid 4 (toSource attrs) False]
        else replicateM_ 4 do
          chooseOneM iid do
            labeled "Take Resource" $ gainResourcesIfCan iid attrs 1
            labeled "Add Supply" do
              chooseTargetM iid supplyAssets \asset ->
                push $ AddUses (toSource attrs) asset Supply 1
      pure e
    _ -> EmergencyCache3 <$> liftRunMessage msg attrs
