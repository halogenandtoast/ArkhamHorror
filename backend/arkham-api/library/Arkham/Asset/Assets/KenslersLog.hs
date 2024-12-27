module Arkham.Asset.Assets.KenslersLog (kenslersLog) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype KenslersLog = KenslersLog AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kenslersLog :: AssetCard KenslersLog
kenslersLog = asset KenslersLog Cards.kenslersLog

instance HasAbilities KenslersLog where
  getAbilities (KenslersLog a) = [investigateAbility a 1 (assetUseCost a Secret 1) ControlsThis]

instance RunMessage KenslersLog where
  runMessage msg a@(KenslersLog attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        labeled "Investigate with a base {intellect} skill of 5"
          $ skillTestModifier sid (attrs.ability 1) iid (BaseSkillOf #intellect 5)
        labeled "Get +2 {intellect} for this investigation"
          $ skillTestModifier sid (attrs.ability 1) iid (SkillModifier #intellect 2)
      skillTestModifier sid (attrs.ability 1) iid (DiscoveredClues 1)
      investigate sid iid (attrs.ability 1)
      pure a
    _ -> KenslersLog <$> liftRunMessage msg attrs
