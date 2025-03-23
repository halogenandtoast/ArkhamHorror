module Arkham.Asset.Assets.EnchantedBlade (enchantedBlade) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers hiding (skillTestModifiers)

newtype EnchantedBlade = EnchantedBlade AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enchantedBlade :: AssetCard EnchantedBlade
enchantedBlade = asset EnchantedBlade Cards.enchantedBlade

getPaidUse :: Payment -> Bool
getPaidUse (UsesPayment n) = n > 0
getPaidUse (Payments ps) = any getPaidUse ps
getPaidUse _ = False

instance HasAbilities EnchantedBlade where
  getAbilities (EnchantedBlade attrs) =
    [ withAdditionalCost (UpTo (Fixed 1) $ assetUseCost attrs Charge 1)
        $ restricted attrs 1 ControlsThis fightAction_
    ]

instance RunMessage EnchantedBlade where
  runMessage msg a@(EnchantedBlade attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (getPaidUse -> paidUse) -> do
      let amount = if paidUse then 2 else 1
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid source iid $ [SkillModifier #combat amount] <> [DamageDealt 1 | paidUse]
      chooseFightEnemy sid iid source
      pure a
    _ -> EnchantedBlade <$> liftRunMessage msg attrs
