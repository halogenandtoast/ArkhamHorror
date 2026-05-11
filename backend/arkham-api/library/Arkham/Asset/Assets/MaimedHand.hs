module Arkham.Asset.Assets.MaimedHand (maimedHand, MaimedHand (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement

newtype MaimedHand = MaimedHand AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maimedHand :: AssetCard MaimedHand
maimedHand = assetWith MaimedHand Cards.maimedHand (canLeavePlayByNormalMeansL .~ False)

instance HasAbilities MaimedHand where
  getAbilities (MaimedHand attrs) = [restrictedAbility attrs 1 OnSameLocation $ ActionAbility mempty Nothing (ActionCost 2)]

instance HasModifiersFor MaimedHand where
  getModifiersFor (MaimedHand a) = case a.placement of
    InThreatArea iid -> modified_ a iid [HealthModifier (-1)]
    _ -> pure mempty

instance RunMessage MaimedHand where
  runMessage msg t@(MaimedHand attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOrRunOneM iid do
        (cardI18n $ labeled' "maimedHand.putMaimedHandIntoPlayInYourThreatArea") do
          place attrs (InThreatArea iid)
        whenM (lift $ can.shuffle.deck iid) do
          (cardI18n $ labeled' "maimedHand.take1DamageAndShuffleItIntoYourDeck") do
            assignDamage iid attrs 1
            shuffleIntoDeck iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> MaimedHand <$> liftRunMessage msg attrs
