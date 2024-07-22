module Arkham.Asset.Cards.Shrivelling5 (Shrivelling5 (..), shrivelling5) where

import Arkham.Ability
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.EffectMetadata
import Arkham.Fight
import Arkham.Prelude

newtype Shrivelling5 = Shrivelling5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shrivelling5 :: AssetCard Shrivelling5
shrivelling5 = asset Shrivelling5 Cards.shrivelling5

instance HasAbilities Shrivelling5 where
  getAbilities (Shrivelling5 a) =
    [restrictedAbility a 1 ControlsThis $ fightAction $ assetUseCost a Charge 1]

instance RunMessage Shrivelling5 where
  runMessage msg a@(Shrivelling5 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseFight <-
        leftOr <$> aspect iid source (#willpower `InsteadOf` #combat) (mkChooseFight sid iid source)
      -- \^ reusing shrivelling(0)'s effect with a damage override
      pushAll
        $ [ skillTestModifiers sid attrs iid [SkillModifier #willpower 3, DamageDealt 2]
          , createCardEffect Cards.shrivelling (effectInt 2) source sid
          ]
        <> chooseFight
      pure a
    _ -> Shrivelling5 <$> runMessage msg attrs
