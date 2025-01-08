module Arkham.Enemy.Cards.TheWingedSerpent (theWingedSerpent, TheWingedSerpent (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher

newtype TheWingedSerpent = TheWingedSerpent EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWingedSerpent :: EnemyCard TheWingedSerpent
theWingedSerpent =
  enemyWith TheWingedSerpent Cards.theWingedSerpent (8, Static 0, 5) (1, 1)
    $ (spawnAtL ?~ SpawnAt (LocationWithTitle "Mouth of K'n-yan"))
    . (healthL .~ Nothing)

instance HasModifiersFor TheWingedSerpent where
  getModifiersFor (TheWingedSerpent a) = modifySelf a [CannotBeDefeated, CannotMakeAttacksOfOpportunity]

instance HasAbilities TheWingedSerpent where
  getAbilities (TheWingedSerpent a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ PlacedCounterOnLocation #after "Mouth of K'n-yan" AnySource #resource AnyValue

instance RunMessage TheWingedSerpent where
  runMessage msg e@(TheWingedSerpent attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      exhaustThis attrs
      roundModifier attrs attrs DoesNotReadyDuringUpkeep
      pure e
    _ -> TheWingedSerpent <$> liftRunMessage msg attrs
