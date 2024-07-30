module Arkham.Asset.Cards.VowOfDrzytelech (vowOfDrzytelech, VowOfDrzytelech (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Message.Lifted.Choose

newtype VowOfDrzytelech = VowOfDrzytelech AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vowOfDrzytelech :: AssetCard VowOfDrzytelech
vowOfDrzytelech = assetWith VowOfDrzytelech Cards.vowOfDrzytelech (canLeavePlayByNormalMeansL .~ False)

instance HasAbilities VowOfDrzytelech where
  getAbilities (VowOfDrzytelech attrs) = [restrictedAbility attrs 1 OnSameLocation $ ActionAbility [] (ActionCost 2)]

instance HasModifiersFor VowOfDrzytelech where
  getModifiersFor (InvestigatorTarget iid) (VowOfDrzytelech attrs) | iid `elem` attrs.inThreatAreaOf = do
    modified attrs [SanityModifier (-1)]
  getModifiersFor _ _ = pure []

instance RunMessage VowOfDrzytelech where
  runMessage msg t@(VowOfDrzytelech attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOrRunOneM iid do
        labeled "Put Vow of Drzytelech into play in your threat area" $ putCardIntoPlay iid attrs
        whenM (lift $ can.shuffle.deck iid) do
          labeled "Take 1 damage and shuffle it into your deck" do
            assignDamage iid attrs 1
            shuffleIntoDeck iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> VowOfDrzytelech <$> liftRunMessage msg attrs
