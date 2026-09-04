module Arkham.Homebrew.CircusExMortis.Enemies.ToweringDarkYoung_068 (toweringDarkYoung_068) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Enemy.Import.Lifted
import Arkham.Fight
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype ToweringDarkYoung_068 = ToweringDarkYoung_068 EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toweringDarkYoung_068 :: EnemyCard ToweringDarkYoung_068
toweringDarkYoung_068 = enemy ToweringDarkYoung_068 Cards.toweringDarkYoung_068

instance HasModifiersFor ToweringDarkYoung_068 where
  getModifiersFor (ToweringDarkYoung_068 a) = modifySelf a [AddKeyword Keyword.Massive]

instance HasAbilities ToweringDarkYoung_068 where
  getAbilities (ToweringDarkYoung_068 a) =
    extend1 a
      $ skillTestAbility
      $ mkAbility a 1
      $ freeReaction
      $ EnemyWouldAttack #when You (CancelableEnemyAttack AnyEnemyAttack) (be a)

instance RunMessage ToweringDarkYoung_068 where
  runMessage msg e@(ToweringDarkYoung_068 attrs) = runQueueT $ case msg of
    -- The attack is made "as if it were at your location": the fight override
    -- skips the co-location requirement, which this enemy is at no location to
    -- meet.
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) attrs
        $ AlternateSuccess (ProxyTarget (toTarget attrs) (toTarget attrs))
      pushM $ withFightOverride <$> mkFightEnemy sid iid (attrs.ability 1) attrs
      pure e
    Successful (Action.Fight, _) _ _ (ProxyTarget (isTarget attrs -> True) _) _ -> do
      for_ (enemyAttacking attrs) $ cancelAttack (attrs.ability 1)
      pure e
    _ -> ToweringDarkYoung_068 <$> liftRunMessage msg attrs
