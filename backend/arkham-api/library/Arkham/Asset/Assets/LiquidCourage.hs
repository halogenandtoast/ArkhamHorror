module Arkham.Asset.Assets.LiquidCourage (liquidCourage) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype LiquidCourage = LiquidCourage AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

liquidCourage :: AssetCard LiquidCourage
liquidCourage = asset LiquidCourage Cards.liquidCourage

instance HasAbilities LiquidCourage where
  getAbilities (LiquidCourage x) =
    [ controlled x 1 (exists (HealableInvestigator (toSource x) #horror $ colocatedWithMatch You))
        $ actionAbilityWithCost (assetUseCost x Supply 1)
    ]

instance RunMessage LiquidCourage where
  runMessage msg a@(LiquidCourage attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      iids <- select $ HealableInvestigator (toSource attrs) #horror $ colocatedWith iid
      sid <- getRandom
      chooseOrRunOneM iid do
        targets iids \iid' -> do
          push $ HealHorrorWithAdditional (toTarget iid') (toSource attrs) 1
          beginSkillTest sid iid' (attrs.ability 1) iid' #willpower (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ AdditionalHealHorror (toTarget iid) (toSource attrs) 1
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ AdditionalHealHorror (toTarget iid) (toSource attrs) 0
      randomDiscard iid (toSource attrs)
      pure a
    _ -> LiquidCourage <$> liftRunMessage msg attrs
