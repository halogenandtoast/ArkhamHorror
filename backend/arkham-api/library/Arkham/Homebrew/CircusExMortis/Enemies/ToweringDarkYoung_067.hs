module Arkham.Homebrew.CircusExMortis.Enemies.ToweringDarkYoung_067 (toweringDarkYoung_067) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Window (getAttackDetails)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype ToweringDarkYoung_067 = ToweringDarkYoung_067 EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toweringDarkYoung_067 :: EnemyCard ToweringDarkYoung_067
toweringDarkYoung_067 = enemy ToweringDarkYoung_067 Cards.toweringDarkYoung_067

instance HasModifiersFor ToweringDarkYoung_067 where
  getModifiersFor (ToweringDarkYoung_067 a) = modifySelf a [AddKeyword Keyword.Massive]

instance HasAbilities ToweringDarkYoung_067 where
  getAbilities (ToweringDarkYoung_067 a) =
    extend1 a
      $ mkAbility a 1
      $ triggered (EnemyWouldAttack #when You (CancelableEnemyAttack AnyEnemyAttack) (be a))
      $ DiscardFromCost 1 (FromHandOf You <> FromPlayAreaOf You) (#asset <> NonWeakness)

instance RunMessage ToweringDarkYoung_067 where
  runMessage msg e@(ToweringDarkYoung_067 attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getAttackDetails -> details) _ -> do
      cancelAttack (attrs.ability 1) details
      pure e
    _ -> ToweringDarkYoung_067 <$> liftRunMessage msg attrs
