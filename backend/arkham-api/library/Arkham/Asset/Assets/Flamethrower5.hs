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
import Arkham.I18n
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
      chooseOneM iid $ cardI18n $ scope "flamethrower5" do
        labeled "standardDamage" $ push $ DealDamage (EnemyTarget eid) $ attack attrs 1
        -- One message per enemy, not per point: each DealDamage raises its own
        -- would-take-damage window, and cards like Mimetic Nemesis trigger off it.
        labeled "assignAmongEngaged"
          $ chooseEnemyAmounts iid ("$" <> labelKey "assignAmongEngaged") damage engaged attrs
      pure a
    ResolveAmounts _ choices (isTarget attrs -> True) -> do
      let assignments = [(EnemyId nu.nuUUID, n) | (nu, n) <- choices, n > 0]
      for_ assignments \(eid, n) ->
        push $ DealDamage (EnemyTarget eid) $ delayDamage $ isDirect $ attack attrs n
      for_ assignments \(eid, _) -> checkDefeated attrs eid
      pure a
    _ -> Flamethrower5 <$> liftRunMessage msg attrs
