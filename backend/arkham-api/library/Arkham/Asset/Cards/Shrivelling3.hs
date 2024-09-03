module Arkham.Asset.Cards.Shrivelling3 (Shrivelling3 (..), shrivelling3) where

import Arkham.Ability
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Prelude

newtype Shrivelling3 = Shrivelling3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shrivelling3 :: AssetCard Shrivelling3
shrivelling3 = asset Shrivelling3 Cards.shrivelling3

instance HasAbilities Shrivelling3 where
  getAbilities (Shrivelling3 a) =
    [restrictedAbility a 1 ControlsThis $ fightAction $ assetUseCost a Charge 1]

instance RunMessage Shrivelling3 where
  runMessage msg a@(Shrivelling3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseFight <-
        leftOr <$> aspect iid source (#willpower `InsteadOf` #combat) (mkChooseFight sid iid source)
      -- reusing shrivelling(0)'s effect
      pushAll
        $ [ skillTestModifiers sid attrs iid [SkillModifier #willpower 2, DamageDealt 1]
          , createCardEffect Cards.shrivelling Nothing source sid
          ]
        <> chooseFight
      pure a
    _ -> Shrivelling3 <$> runMessage msg attrs
