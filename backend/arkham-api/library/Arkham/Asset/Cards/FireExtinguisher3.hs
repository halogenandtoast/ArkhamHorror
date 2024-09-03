module Arkham.Asset.Cards.FireExtinguisher3 where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Fight
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Modifier

newtype FireExtinguisher3 = FireExtinguisher3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fireExtinguisher3 :: AssetCard FireExtinguisher3
fireExtinguisher3 = asset FireExtinguisher3 Cards.fireExtinguisher3

instance HasAbilities FireExtinguisher3 where
  getAbilities (FireExtinguisher3 a) =
    [ restrictedAbility a 1 ControlsThis fightAction_
    , notSkillTestAbility
        $ restrictedAbility a 2 ControlsThis
        $ evadeAction
        $ OrCost [discardCost a, exileCost a]
    ]

instance RunMessage FireExtinguisher3 where
  runMessage msg a@(FireExtinguisher3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid source iid [SkillModifier #combat 1, DamageDealt 1]
      pushM $ mkChooseFight sid iid source
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      enemies <- select $ enemyAtLocationWith iid <> NonEliteEnemy <> EnemyWithoutModifier CannotBeEvaded
      pushAll $ map (EnemyEvaded iid) enemies
      when attrs.exiled do
        pushAll $ map (Discard (Just iid) (attrs.ability 1) . toTarget) enemies
      pure a
    _ -> FireExtinguisher3 <$> liftRunMessage msg attrs
