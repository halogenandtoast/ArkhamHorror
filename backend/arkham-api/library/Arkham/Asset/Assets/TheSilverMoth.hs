module Arkham.Asset.Assets.TheSilverMoth (theSilverMoth, TheSilverMoth (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype TheSilverMoth = TheSilverMoth AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSilverMoth :: AssetCard TheSilverMoth
theSilverMoth = assetWith TheSilverMoth Cards.theSilverMoth (canLeavePlayByNormalMeansL .~ False)

instance HasAbilities TheSilverMoth where
  getAbilities (TheSilverMoth attrs) = [restrictedAbility attrs 1 OnSameLocation $ ActionAbility [] (ActionCost 2)]

instance HasModifiersFor TheSilverMoth where
  getModifiersFor (TheSilverMoth a) = case a.placement of
    InThreatArea iid -> modified_ a iid [SanityModifier (-1)]
    _ -> pure mempty

instance RunMessage TheSilverMoth where
  runMessage msg t@(TheSilverMoth attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOrRunOneM iid do
        labeled "Put The Silver Moth into play in your threat area" do
          putCardIntoPlay iid attrs
          checkDefeated attrs iid
        whenM (lift $ can.shuffle.deck iid) do
          labeled "Take 1 horror and shuffle it into your deck" do
            assignHorror iid attrs 1
            shuffleIntoDeck iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> TheSilverMoth <$> liftRunMessage msg attrs
