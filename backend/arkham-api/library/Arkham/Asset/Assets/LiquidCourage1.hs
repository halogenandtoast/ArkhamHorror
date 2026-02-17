module Arkham.Asset.Assets.LiquidCourage1 (liquidCourage1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype LiquidCourage1 = LiquidCourage1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

liquidCourage1 :: AssetCard LiquidCourage1
liquidCourage1 = asset LiquidCourage1 Cards.liquidCourage1

instance HasAbilities LiquidCourage1 where
  getAbilities (LiquidCourage1 x) =
    [ controlled x 1 (exists $ HealableInvestigator (toSource x) #horror $ colocatedWithMatch You)
        $ actionAbilityWithCost (assetUseCost x Supply 1)
    ]

instance RunMessage LiquidCourage1 where
  runMessage msg a@(LiquidCourage1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      iids <- select $ HealableInvestigator (attrs.ability 1) #horror $ colocatedWith iid
      sid <- getRandom
      chooseOrRunOneM iid do
        targets iids \iid' -> do
          push $ HealHorrorWithAdditional (toTarget iid') (toSource attrs) 1
          beginSkillTest sid iid' (attrs.ability 1) iid' #willpower (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ AdditionalHealHorror (toTarget iid) (attrs.ability 1) 0
      drawCards iid (attrs.ability 1) 1
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ AdditionalHealHorror (toTarget iid) (attrs.ability 1) 1
      randomDiscard iid (toSource attrs)
      pure a
    _ -> LiquidCourage1 <$> liftRunMessage msg attrs
