module Arkham.Enemy.Cards.ProfessorNathanielTaylor (professorNathanielTaylor) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype ProfessorNathanielTaylor = ProfessorNathanielTaylor EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

professorNathanielTaylor :: EnemyCard ProfessorNathanielTaylor
professorNathanielTaylor =
  enemyWith ProfessorNathanielTaylor Cards.professorNathanielTaylor (3, Static 4, 4) (0, 1)
    $ spawnAtL
    ?~ SpawnAt "Outskirts of Cairo"

instance HasAbilities ProfessorNathanielTaylor where
  getAbilities (ProfessorNathanielTaylor a) =
    extend1 a
      $ restricted a 1 OnSameLocation
      $ parleyAction (GroupSkillIconCost 4 (singleton #intellect) (locationWithEnemy a))

instance RunMessage ProfessorNathanielTaylor where
  runMessage msg e@(ProfessorNathanielTaylor attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      addToVictory iid attrs
      pure e
    _ -> ProfessorNathanielTaylor <$> liftRunMessage msg attrs
