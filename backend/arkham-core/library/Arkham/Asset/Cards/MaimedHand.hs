module Arkham.Asset.Cards.MaimedHand (maimedHand, MaimedHand (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Message.Lifted.Choose

newtype MaimedHand = MaimedHand AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maimedHand :: AssetCard MaimedHand
maimedHand = assetWith MaimedHand Cards.maimedHand (canLeavePlayByNormalMeansL .~ False)

instance HasAbilities MaimedHand where
  getAbilities (MaimedHand attrs) = [restrictedAbility attrs 1 OnSameLocation $ ActionAbility [] (ActionCost 2)]

instance HasModifiersFor MaimedHand where
  getModifiersFor (InvestigatorTarget iid) (MaimedHand attrs) | iid `elem` attrs.inThreatAreaOf = do
    modified attrs [HealthModifier (-1)]
  getModifiersFor _ _ = pure []

instance RunMessage MaimedHand where
  runMessage msg t@(MaimedHand attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOrRunOneM iid do
        labeled "Put Maimed Hand into play in your threat area" $ putCardIntoPlay iid attrs
        whenM (lift $ can.shuffle.deck iid) do
          labeled "Take 1 damage and shuffle it into your deck" do
            assignDamage iid attrs 1
            shuffleIntoDeck iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> MaimedHand <$> liftRunMessage msg attrs
