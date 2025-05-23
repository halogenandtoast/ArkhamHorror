module Arkham.Enemy.Cards.WizardOfTheOrder (wizardOfTheOrder) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype WizardOfTheOrder = WizardOfTheOrder EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wizardOfTheOrder :: EnemyCard WizardOfTheOrder
wizardOfTheOrder =
  enemyWith
    WizardOfTheOrder
    Cards.wizardOfTheOrder
    (4, Static 2, 2)
    (1, 0)
    (spawnAtL ?~ SpawnAt EmptyLocation)

instance HasAbilities WizardOfTheOrder where
  getAbilities (WizardOfTheOrder a) =
    extend1 a $ restricted a 1 CanPlaceDoomOnThis $ forced $ PhaseEnds #when #mythos

instance RunMessage WizardOfTheOrder where
  runMessage msg e@(WizardOfTheOrder attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure e
    _ -> WizardOfTheOrder <$> liftRunMessage msg attrs
