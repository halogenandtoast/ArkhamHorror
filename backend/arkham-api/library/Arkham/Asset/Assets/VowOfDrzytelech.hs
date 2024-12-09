module Arkham.Asset.Assets.VowOfDrzytelech (vowOfDrzytelech, VowOfDrzytelech (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Placement

newtype VowOfDrzytelech = VowOfDrzytelech AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vowOfDrzytelech :: AssetCard VowOfDrzytelech
vowOfDrzytelech = assetWith VowOfDrzytelech Cards.vowOfDrzytelech (canLeavePlayByNormalMeansL .~ False)

instance HasAbilities VowOfDrzytelech where
  getAbilities (VowOfDrzytelech attrs) = [restrictedAbility attrs 1 OnSameLocation $ ActionAbility [] (ActionCost 2)]

instance HasModifiersFor VowOfDrzytelech where
  getModifiersFor (VowOfDrzytelech a) = case a.placement of
    InThreatArea iid -> modified_ a iid [SanityModifier (-1)]
    _ -> pure mempty

instance RunMessage VowOfDrzytelech where
  runMessage msg t@(VowOfDrzytelech attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOrRunOneM iid do
        labeled "Put Vow of Drzytelech into play in your threat area" do
          putCardIntoPlay iid attrs
          checkDefeated attrs iid
        whenM (lift $ can.shuffle.deck iid) do
          labeled "Take 1 horror and shuffle it into your deck" do
            assignHorror iid attrs 1
            shuffleIntoDeck iid attrs
      pure t
    CardEnteredPlay iid card | card.id == attrs.cardId -> do
      place attrs (InThreatArea iid)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> VowOfDrzytelech <$> liftRunMessage msg attrs
