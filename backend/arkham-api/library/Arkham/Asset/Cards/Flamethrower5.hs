module Arkham.Asset.Cards.Flamethrower5 (flamethrower5, Flamethrower5 (..)) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.DamageEffect
import Arkham.Enemy.Types qualified as Field (Field (..))
import Arkham.Fight
import Arkham.Helpers.Investigator
import Arkham.Helpers.Projection
import Arkham.Matcher
import Arkham.Prelude

newtype Flamethrower5 = Flamethrower5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

flamethrower5 :: AssetCard Flamethrower5
flamethrower5 = asset Flamethrower5 Cards.flamethrower5

instance HasAbilities Flamethrower5 where
  getAbilities (Flamethrower5 a) = [restrictedAbility a 1 ControlsThis $ fightAction (assetUseCost a Ammo 1)]

instance RunMessage Flamethrower5 where
  runMessage msg a@(Flamethrower5 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      enemies <- withMaybeMaxField Field.EnemyFight =<< select (enemyEngagedWith iid)
      chooseFight <-
        toMessage . setTarget attrs <$> mkChooseFightMatch sid iid source (oneOf $ map EnemyWithId enemies)
      pushAll
        [ skillTestModifier sid attrs iid (SkillModifier #combat 4)
        , chooseFight
        ]
      pure a
    Successful (Action.Fight, EnemyTarget eid) iid _ (isTarget attrs -> True) _ -> do
      damage <- damageValueFor 4 iid DamageForEnemy
      engaged <- select $ enemyEngagedWith iid
      let toMsg eid' = EnemyDamage eid' $ delayDamage $ isDirect $ attack attrs 1
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ Label "Do standard damage" [EnemyDamage eid $ attack attrs 1]
          , Label "Assign up to 4 damage among enemies engaged with you"
              $ replicate damage
              $ chooseOne player [targetLabel eid' [toMsg eid'] | eid' <- engaged]
          ]
      pure a
    _ -> Flamethrower5 <$> runMessage msg attrs
