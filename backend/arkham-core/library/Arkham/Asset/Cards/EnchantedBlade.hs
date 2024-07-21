module Arkham.Asset.Cards.EnchantedBlade (enchantedBlade, EnchantedBlade (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Prelude

newtype EnchantedBlade = EnchantedBlade AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enchantedBlade :: AssetCard EnchantedBlade
enchantedBlade = asset EnchantedBlade Cards.enchantedBlade

getPaidUse :: Payment -> Bool
getPaidUse (UsesPayment _) = True
getPaidUse (Payments ps) = any getPaidUse ps
getPaidUse _ = False

instance HasAbilities EnchantedBlade where
  getAbilities (EnchantedBlade attrs) =
    [ withAdditionalCost (UpTo (Fixed 1) $ assetUseCost attrs Charge 1)
        $ restrictedAbility attrs 1 ControlsThis
        $ fightAction_
    ]

instance RunMessage EnchantedBlade where
  runMessage msg a@(EnchantedBlade attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (getPaidUse -> paidUse) -> do
      let amount = if paidUse then 2 else 1
      let source = attrs.ability 1
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid source
      pushAll
        [ skillTestModifiers sid attrs iid $ [SkillModifier #combat amount] <> [DamageDealt 1 | paidUse]
        , chooseFight
        ]
      pure a
    _ -> EnchantedBlade <$> runMessage msg attrs
