module Arkham.Asset.Assets.Flamethrower5 (flamethrower5, Flamethrower5 (..)) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.DamageEffect
import Arkham.Enemy.Types qualified as Field (Field (..))
import Arkham.Helpers.Investigator
import Arkham.Helpers.Projection
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype Flamethrower5 = Flamethrower5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

flamethrower5 :: AssetCard Flamethrower5
flamethrower5 = asset Flamethrower5 Cards.flamethrower5

instance HasAbilities Flamethrower5 where
  getAbilities (Flamethrower5 a) =
    [ controlledAbility a 1 (exists $ EnemyIsEngagedWith You <> CanFightEnemy (toSource a))
        $ fightAction (assetUseCost a Ammo 1)
    ]

instance RunMessage Flamethrower5 where
  runMessage msg a@(Flamethrower5 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (SkillModifier #combat 4)
      enemies <- withMaybeMaxField Field.EnemyFight =<< select (enemyEngagedWith iid)
      chooseFightEnemyMatchEdit sid iid (attrs.ability 1) (beOneOf enemies) (setTarget attrs)
      pure a
    Successful (Action.Fight, EnemyTarget eid) iid _ (isTarget attrs -> True) _ -> do
      damage <- damageValueFor 4 iid DamageForEnemy
      engaged <- select $ enemyEngagedWith iid
      let toMsg eid' = EnemyDamage eid' $ delayDamage $ isDirect $ attack attrs 1
      chooseOneM iid do
        labeled "Do standard damage" $ push $ EnemyDamage eid $ attack attrs 1
        labeled "Assign up to 4 damage among enemies engaged with you" do
          replicateM_ damage $ chooseTargetM iid engaged $ push . toMsg
          for_ engaged $ checkDefeated attrs
      pure a
    _ -> Flamethrower5 <$> liftRunMessage msg attrs
