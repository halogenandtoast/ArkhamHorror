module Arkham.Asset.Assets.BackInjury (backInjury, BackInjury (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype BackInjury = BackInjury AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backInjury :: AssetCard BackInjury
backInjury = assetWith BackInjury Cards.backInjury (canLeavePlayByNormalMeansL .~ False)

instance HasAbilities BackInjury where
  getAbilities (BackInjury attrs) = [restrictedAbility attrs 1 OnSameLocation $ ActionAbility [] (ActionCost 2)]

instance HasModifiersFor BackInjury where
  getModifiersFor (BackInjury a) = case a.placement of
    InThreatArea iid -> modified_ a iid [HealthModifier (-1)]
    _ -> pure mempty

instance RunMessage BackInjury where
  runMessage msg t@(BackInjury attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOrRunOneM iid do
        labeled "Put Back Injury into play in your threat area" $ putCardIntoPlay iid attrs
        whenM (lift $ can.shuffle.deck iid) do
          labeled "Take 1 damage and shuffle it into your deck" do
            assignDamage iid attrs 1
            shuffleIntoDeck iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> BackInjury <$> liftRunMessage msg attrs
