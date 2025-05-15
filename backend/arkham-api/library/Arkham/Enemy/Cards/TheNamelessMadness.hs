module Arkham.Enemy.Cards.TheNamelessMadness (theNamelessMadness) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Window (getPassedBy)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype TheNamelessMadness = TheNamelessMadness EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theNamelessMadness :: EnemyCard TheNamelessMadness
theNamelessMadness =
  enemyWith TheNamelessMadness Cards.theNamelessMadness (0, Static 1, 0) (1, 1)
    $ (fightL ?~ GameValueCalculation (PerPlayer 1))
    . (healthL .~ Nothing)
    . (evadeL ?~ GameValueCalculation (PerPlayer 1))

instance HasModifiersFor TheNamelessMadness where
  getModifiersFor (TheNamelessMadness a) = modifySelf a [CannotBeDamaged]

instance HasAbilities TheNamelessMadness where
  getAbilities (TheNamelessMadness a) =
    extend1 a
      $ restricted a 1 (exists $ enemyIs Cards.theNamelessMadness <> ReadyEnemy)
      $ forced
      $ SkillTestResult
        #after
        You
        (oneOf [WhileEvadingAnEnemy (be a), WhileAttackingAnEnemy (be a)])
        (SuccessResult (atLeast 1))

instance RunMessage TheNamelessMadness where
  runMessage msg e@(TheNamelessMadness attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (getPassedBy -> n) _ -> do
      doStep n msg
      pure e
    DoStep n msg'@(UseThisAbility iid (isSource attrs -> True) 1) | n > 0 -> do
      xs <- select $ NearestEnemyTo iid $ enemyIs Cards.theNamelessMadness <> ReadyEnemy
      unless (null xs) do
        if length xs <= n
          then do
            for_ xs exhaustEnemy
            doStep (n - length xs) msg'
          else chooseNM iid n do
            targets xs exhaustEnemy
      pure e
    _ -> TheNamelessMadness <$> liftRunMessage msg attrs
