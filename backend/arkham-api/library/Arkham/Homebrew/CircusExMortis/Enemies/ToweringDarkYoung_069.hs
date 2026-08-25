module Arkham.Homebrew.CircusExMortis.Enemies.ToweringDarkYoung_069 (toweringDarkYoung_069) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype ToweringDarkYoung_069 = ToweringDarkYoung_069 EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toweringDarkYoung_069 :: EnemyCard ToweringDarkYoung_069
toweringDarkYoung_069 = enemy ToweringDarkYoung_069 Cards.toweringDarkYoung_069

instance HasModifiersFor ToweringDarkYoung_069 where
  getModifiersFor (ToweringDarkYoung_069 a) = modifySelf a [AddKeyword Keyword.Massive]

instance HasAbilities ToweringDarkYoung_069 where
  getAbilities (ToweringDarkYoung_069 a) =
    extend1 a
      $ skillTestAbility
      $ mkAbility a 1
      $ freeReaction
      $ EnemyWouldAttack #when You (CancelableEnemyAttack AnyEnemyAttack) (be a)

instance RunMessage ToweringDarkYoung_069 where
  runMessage msg e@(ToweringDarkYoung_069 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      withLocationOf iid \lid -> do
        skillTestModifier sid (attrs.ability 1) lid (AlternateSuccessfullInvestigation $ toTarget attrs)
        investigate sid iid (attrs.ability 1)
      pure e
    Successful (Action.Investigate, _) _ _ (isTarget attrs -> True) _ -> do
      for_ (enemyAttacking attrs) $ cancelAttack (attrs.ability 1)
      pure e
    _ -> ToweringDarkYoung_069 <$> liftRunMessage msg attrs
