module Arkham.Asset.Cards.HealingWords3 where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher

newtype HealingWords3 = HealingWords3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

healingWords3 :: AssetCard HealingWords3
healingWords3 = asset HealingWords3 Cards.healingWords3

instance HasAbilities HealingWords3 where
  getAbilities (HealingWords3 a) =
    [ controlledAbility a 1 (exists $ HealableInvestigator (a.ability 1) #damage AtYourLocation)
        $ actionAbilityWithCost (assetUseCost a Charge 1)
    ]

instance RunMessage HealingWords3 where
  runMessage msg a@(HealingWords3 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ DoStep 2 msg
      pure a
    DoStep n msg'@(UseThisAbility iid (isSource attrs -> True) 1) | n > 0 -> do
      targets <- select $ HealableInvestigator (attrs.ability 1) #damage $ colocatedWith iid
      unless (null targets) do
        chooseOrRunOne iid [targetLabel i [HealDamage (toTarget i) (attrs.ability 1) 1] | i <- targets]
        push $ DoStep (n - 1) msg'
      pure a
    _ -> HealingWords3 <$> lift (runMessage msg attrs)
