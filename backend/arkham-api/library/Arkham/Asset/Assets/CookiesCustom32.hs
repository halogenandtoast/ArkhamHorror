module Arkham.Asset.Assets.CookiesCustom32 (cookiesCustom32) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype CookiesCustom32 = CookiesCustom32 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cookiesCustom32 :: AssetCard CookiesCustom32
cookiesCustom32 = asset CookiesCustom32 Cards.cookiesCustom32

instance HasAbilities CookiesCustom32 where
  getAbilities (CookiesCustom32 a) = [fightAbility a 1 (assetUseCost a Ammo 1) ControlsThis]

instance RunMessage CookiesCustom32 where
  runMessage msg a@(CookiesCustom32 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        labeled "Fight with a base {combat} skill of 5"
          $ skillTestModifier sid (attrs.ability 1) iid (BaseSkillOf #combat 5)
        labeled "Get +2 {combat} for this attack"
          $ skillTestModifier sid (attrs.ability 1) iid (SkillModifier #combat 2)
      skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    _ -> CookiesCustom32 <$> liftRunMessage msg attrs
