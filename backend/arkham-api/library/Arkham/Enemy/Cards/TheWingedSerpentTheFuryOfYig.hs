module Arkham.Enemy.Cards.TheWingedSerpentTheFuryOfYig (theWingedSerpentTheFuryOfYig) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher

newtype TheWingedSerpentTheFuryOfYig = TheWingedSerpentTheFuryOfYig EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theWingedSerpentTheFuryOfYig :: EnemyCard TheWingedSerpentTheFuryOfYig
theWingedSerpentTheFuryOfYig =
  enemyWith TheWingedSerpentTheFuryOfYig Cards.theWingedSerpentTheFuryOfYig (4, Static 1, 4) (1, 1)
    $ (spawnAtL ?~ SpawnAt (LocationWithTitle "Mouth of K'n-yan"))
    . (healthL .~ Nothing)

instance HasModifiersFor TheWingedSerpentTheFuryOfYig where
  getModifiersFor (TheWingedSerpentTheFuryOfYig a) = modifySelf a [CannotBeDefeated, CannotMakeAttacksOfOpportunity]

instance RunMessage TheWingedSerpentTheFuryOfYig where
  runMessage msg (TheWingedSerpentTheFuryOfYig attrs) = runQueueT $ case msg of
    _ -> TheWingedSerpentTheFuryOfYig <$> liftRunMessage msg attrs
