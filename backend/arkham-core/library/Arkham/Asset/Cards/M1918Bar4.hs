module Arkham.Asset.Cards.M1918Bar4 (m1918Bar4, M1918Bar4 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Matcher
import Arkham.Prelude

newtype M1918Bar4 = M1918Bar4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

m1918Bar4 :: AssetCard M1918Bar4
m1918Bar4 = asset M1918Bar4 Cards.m1918Bar4

instance HasAbilities M1918Bar4 where
  getAbilities (M1918Bar4 a) =
    [restrictedAbility a 1 ControlsThis $ fightAction $ UseCostUpTo (AssetWithId $ toId a) Ammo 1 5]

totalUses :: Payment -> Int
totalUses (Payments ps) = sum $ map totalUses ps
totalUses (UsesPayment n) = n
totalUses _ = 0

instance RunMessage M1918Bar4 where
  runMessage msg a@(M1918Bar4 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ payments -> do
      let uses = totalUses payments
      let source = attrs.ability 1
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid source
      pushAll
        [ skillTestModifiers sid source iid [SkillModifier #combat uses, DamageDealt (uses - 1)]
        , chooseFight
        ]
      pure a
    _ -> M1918Bar4 <$> runMessage msg attrs
