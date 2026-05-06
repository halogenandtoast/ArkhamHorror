module Arkham.Event.Events.CustomGrip (customGrip) where

import Arkham.Ability
import Arkham.Cost.Status qualified as Cost
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Upgrade

newtype CustomGrip = CustomGrip EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

customGrip :: EventCard CustomGrip
customGrip = event CustomGrip Cards.customGrip

instance HasAbilities CustomGrip where
  getAbilities (CustomGrip a) =
    case a.attachedTo of
      Just (AssetTarget _) -> [controlled a 1 (DuringTurn You) $ FastAbility Free]
      _ -> []

instance RunMessage CustomGrip where
  runMessage msg e@(CustomGrip attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      assets <- getUpgradeTargets iid $ assetControlledBy iid <> #firearm
      chooseTargetM iid assets \asset -> place attrs $ AttachedToAsset asset Nothing
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ attrs.attachedTo \case
        AssetTarget aid -> do
          returnToHand iid aid
          toDiscardBy iid (attrs.ability 1) attrs
          doStep 1 msg
        _ -> pure ()
      pure e
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      cards <- select $ PlayableCard (Cost.UnpaidCost Cost.NoAction) $ inHandOf ForPlay iid <> basic #firearm
      unless (null cards) do
        chooseOrRunOneM iid do
          labeled "Do not play a Firearm" nothing
          targets cards $ playCardPayingCost iid
      pure e
    _ -> CustomGrip <$> liftRunMessage msg attrs
