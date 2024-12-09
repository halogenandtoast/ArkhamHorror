module Arkham.Asset.Assets.MaimedHand (maimedHand, MaimedHand (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Placement

newtype MaimedHand = MaimedHand AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maimedHand :: AssetCard MaimedHand
maimedHand = assetWith MaimedHand Cards.maimedHand (canLeavePlayByNormalMeansL .~ False)

instance HasAbilities MaimedHand where
  getAbilities (MaimedHand attrs) = [restrictedAbility attrs 1 OnSameLocation $ ActionAbility [] (ActionCost 2)]

instance HasModifiersFor MaimedHand where
  getModifiersFor (MaimedHand a) = case a.placement of
    InThreatArea iid -> modified_ a iid [HealthModifier (-1)]
    _ -> pure mempty

instance RunMessage MaimedHand where
  runMessage msg t@(MaimedHand attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOrRunOneM iid do
        labeled "Put Maimed Hand into play in your threat area" $ place attrs (InThreatArea iid)
        whenM (lift $ can.shuffle.deck iid) do
          labeled "Take 1 damage and shuffle it into your deck" do
            assignDamage iid attrs 1
            shuffleIntoDeck iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> MaimedHand <$> liftRunMessage msg attrs
