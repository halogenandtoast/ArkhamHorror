module Arkham.Asset.Cards.EnchantedBlade
  ( enchantedBlade
  , EnchantedBlade(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

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
    [ restrictedAbility attrs 1 ControlsThis
        $ ActionAbility (Just Action.Fight)
        $ Costs
            [ActionCost 1, UpTo 1 (UseCost (AssetWithId $ toId attrs) Charge 1)]
    ]

instance RunMessage EnchantedBlade where
  runMessage msg a@(EnchantedBlade attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (getPaidUse -> paidUse) ->
      do
        pushAll
          [ skillTestModifiers
            attrs
            (InvestigatorTarget iid)
            ([SkillModifier SkillCombat (if paidUse then 2 else 1)]
            <> [ DamageDealt 1 | paidUse ]
            )
          , ChooseFightEnemy iid (toSource attrs) Nothing SkillCombat mempty False
          ]
        pure a
    _ -> EnchantedBlade <$> runMessage msg attrs
