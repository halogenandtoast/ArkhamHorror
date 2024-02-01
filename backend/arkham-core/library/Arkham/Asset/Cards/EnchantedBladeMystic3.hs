module Arkham.Asset.Cards.EnchantedBladeMystic3 (
  enchantedBladeMystic3,
  EnchantedBladeMystic3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype EnchantedBladeMystic3 = EnchantedBladeMystic3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

enchantedBladeMystic3 :: AssetCard EnchantedBladeMystic3
enchantedBladeMystic3 = asset EnchantedBladeMystic3 Cards.enchantedBladeMystic3

getUsesPaid :: Payment -> Int
getUsesPaid (UsesPayment n) = n
getUsesPaid (Payments ps) = sum $ map getUsesPaid ps
getUsesPaid _ = 0

instance HasAbilities EnchantedBladeMystic3 where
  getAbilities (EnchantedBladeMystic3 attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ ActionAbility ([Action.Fight])
        $ Costs
          [ActionCost 1, UpTo 2 (UseCost (AssetWithId $ toId attrs) Charge 1)]
    ]

instance RunMessage EnchantedBladeMystic3 where
  runMessage msg a@(EnchantedBladeMystic3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (getUsesPaid -> usesPaid) ->
      do
        pushAll
          [ skillTestModifiers
              attrs
              (InvestigatorTarget iid)
              ( [SkillModifier SkillCombat (2 + usesPaid)]
                  <> [DamageDealt usesPaid | usesPaid > 0]
              )
          , ChooseFightEnemy
              iid
              (toSource attrs)
              Nothing
              SkillCombat
              mempty
              False
          ]
        pure a
    _ -> EnchantedBladeMystic3 <$> runMessage msg attrs
