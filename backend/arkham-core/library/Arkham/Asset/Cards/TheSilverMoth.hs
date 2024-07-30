module Arkham.Asset.Cards.TheSilverMoth (theSilverMoth, TheSilverMoth (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Message.Lifted.Choose

newtype TheSilverMoth = TheSilverMoth AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSilverMoth :: AssetCard TheSilverMoth
theSilverMoth = assetWith TheSilverMoth Cards.theSilverMoth (canLeavePlayByNormalMeansL .~ False)

instance HasAbilities TheSilverMoth where
  getAbilities (TheSilverMoth attrs) = [restrictedAbility attrs 1 OnSameLocation $ ActionAbility [] (ActionCost 2)]

instance HasModifiersFor TheSilverMoth where
  getModifiersFor (InvestigatorTarget iid) (TheSilverMoth attrs) | iid `elem` attrs.inThreatAreaOf = do
    modified attrs [SanityModifier (-1)]
  getModifiersFor _ _ = pure []

instance RunMessage TheSilverMoth where
  runMessage msg t@(TheSilverMoth attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOrRunOneM iid do
        labeled "Put The Silver Moth into play in your threat area" $ putCardIntoPlay iid attrs
        whenM (lift $ can.shuffle.deck iid) do
          labeled "Take 1 damage and shuffle it into your deck" do
            assignDamage iid attrs 1
            shuffleIntoDeck iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> TheSilverMoth <$> liftRunMessage msg attrs
