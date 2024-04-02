module Arkham.Asset.Cards.EnchantedBladeMystic3 (enchantedBladeMystic3, EnchantedBladeMystic3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Prelude

newtype EnchantedBladeMystic3 = EnchantedBladeMystic3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enchantedBladeMystic3 :: AssetCard EnchantedBladeMystic3
enchantedBladeMystic3 = asset EnchantedBladeMystic3 Cards.enchantedBladeMystic3

getUsesPaid :: Payment -> Int
getUsesPaid (UsesPayment n) = n
getUsesPaid (Payments ps) = sum $ map getUsesPaid ps
getUsesPaid _ = 0

instance HasAbilities EnchantedBladeMystic3 where
  getAbilities (EnchantedBladeMystic3 attrs) =
    [restrictedAbility attrs 1 ControlsThis $ fightAction $ UpTo 2 (assetUseCost attrs Charge 1)]

instance RunMessage EnchantedBladeMystic3 where
  runMessage msg a@(EnchantedBladeMystic3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (getUsesPaid -> usesPaid) -> do
      let source = attrs.ability 1
      chooseFight <- toMessage <$> mkChooseFight iid source
      pushAll
        [ skillTestModifiers source iid
            $ [SkillModifier #combat (2 + usesPaid)]
            <> [DamageDealt usesPaid | usesPaid > 0]
        , chooseFight
        ]
      pure a
    _ -> EnchantedBladeMystic3 <$> runMessage msg attrs
