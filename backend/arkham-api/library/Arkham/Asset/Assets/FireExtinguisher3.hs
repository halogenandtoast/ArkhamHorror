module Arkham.Asset.Assets.FireExtinguisher3 where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Modifier

newtype FireExtinguisher3 = FireExtinguisher3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fireExtinguisher3 :: AssetCard FireExtinguisher3
fireExtinguisher3 = asset FireExtinguisher3 Cards.fireExtinguisher3

instance HasAbilities FireExtinguisher3 where
  getAbilities (FireExtinguisher3 a) =
    [ restricted a 1 ControlsThis fightAction_
    , notSkillTestAbility
        $ restricted a 2 ControlsThis
        $ evadeAction
        $ OrCost [discardCost a, exileCost a]
    ]

instance RunMessage FireExtinguisher3 where
  runMessage msg a@(FireExtinguisher3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid source iid [SkillModifier #combat 1, DamageDealt 1]
      chooseFightEnemy sid iid source
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      enemies <- select $ enemyAtLocationWith iid <> EnemyWithoutModifier CannotBeEvaded
      for_ enemies (automaticallyEvadeEnemy iid)
      when attrs.exiled do
        for_ enemies \enemy -> do
          whenMatch enemy NonEliteEnemy $ toDiscardBy iid (attrs.ability 1) enemy
      pure a
    _ -> FireExtinguisher3 <$> liftRunMessage msg attrs
