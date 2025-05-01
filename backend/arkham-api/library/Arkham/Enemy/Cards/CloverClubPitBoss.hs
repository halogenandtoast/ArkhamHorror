module Arkham.Enemy.Cards.CloverClubPitBoss (cloverClubPitBoss) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype CloverClubPitBoss = CloverClubPitBoss EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloverClubPitBoss :: EnemyCard CloverClubPitBoss
cloverClubPitBoss =
  enemyWith CloverClubPitBoss Cards.cloverClubPitBoss (3, Static 4, 3) (2, 0)
    $ preyL
    .~ Prey (InvestigatorWithHighestSkill #intellect UneliminatedInvestigator)

instance HasAbilities CloverClubPitBoss where
  getAbilities (CloverClubPitBoss x) =
    extend1 x $ restricted x 1 OnSameLocation $ forced $ GainsClues #after You AnyValue

instance RunMessage CloverClubPitBoss where
  runMessage msg e@(CloverClubPitBoss attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      when attrs.exhausted $ readyThis attrs
      enemyEngageInvestigator attrs iid
      attackIfEngaged attrs (Just iid)
      pure e
    _ -> CloverClubPitBoss <$> liftRunMessage msg attrs
