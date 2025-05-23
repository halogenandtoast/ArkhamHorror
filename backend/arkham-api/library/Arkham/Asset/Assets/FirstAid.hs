module Arkham.Asset.Assets.FirstAid (firstAid) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Investigator (canHaveDamageHealed, canHaveHorrorHealed)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype FirstAid = FirstAid AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

firstAid :: AssetCard FirstAid
firstAid = assetWith FirstAid Cards.firstAid discardWhenNoUses

instance HasAbilities FirstAid where
  getAbilities (FirstAid x) = [controlled x 1 criteria $ actionAbilityWithCost $ assetUseCost x #supply 1]
   where
    criteria = exists $ mapOneOf healable [#horror, #damage]
    healable hType = HealableInvestigator (x.ability 1) hType $ colocatedWithMatch You

instance RunMessage FirstAid where
  runMessage msg a@(FirstAid attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      investigators <- select $ colocatedWith iid
      chooseOrRunOneM iid do
        targets investigators \i -> do
          chooseOrRunOneM iid do
            whenM (canHaveDamageHealed source i) $ damageLabeled i $ healDamage i source 1
            whenM (canHaveHorrorHealed source i) $ horrorLabeled i $ healHorror i source 1
      pure a
    _ -> FirstAid <$> liftRunMessage msg attrs
