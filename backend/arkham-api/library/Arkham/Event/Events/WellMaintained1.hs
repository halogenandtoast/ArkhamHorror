module Arkham.Event.Events.WellMaintained1 (wellMaintained1) where

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype WellMaintained1 = WellMaintained1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wellMaintained1 :: EventCard WellMaintained1
wellMaintained1 = event WellMaintained1 Cards.wellMaintained1

instance HasAbilities WellMaintained1 where
  getAbilities (WellMaintained1 a) = case a.placement of
    AttachedToAsset aid _ ->
      [restricted a 1 ControlsThis $ freeReaction (AssetWouldBeDiscarded #after (AssetWithId aid))]
    _ -> []

instance RunMessage WellMaintained1 where
  runMessage msg e@(WellMaintained1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      assets <- select $ assetControlledBy iid <> #item
      chooseTargetM iid assets \asset -> place attrs $ AttachedToAsset asset Nothing
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      case attrs.placement of
        AttachedToAsset aid _ -> do
          otherUpgrades <- select $ EventAttachedToAsset (AssetWithId aid) <> not_ (be attrs) <> #upgrade
          returnToHand iid aid
          for_ otherUpgrades (returnToHand iid)
        _ -> error "Invalid placement"
      pure e
    _ -> WellMaintained1 <$> liftRunMessage msg attrs
