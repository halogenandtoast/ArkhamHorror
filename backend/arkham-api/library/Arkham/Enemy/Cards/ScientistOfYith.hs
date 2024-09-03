module Arkham.Enemy.Cards.ScientistOfYith (
  scientistOfYith,
  ScientistOfYith (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.ScenarioLogKey

newtype ScientistOfYith = ScientistOfYith EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

scientistOfYith :: EnemyCard ScientistOfYith
scientistOfYith =
  enemyWith
    ScientistOfYith
    Cards.scientistOfYith
    (4, Static 3, 1)
    (2, 0)
    (spawnAtL ?~ SpawnAt (LocationWithTitle "Laboratory of the Great Race"))

instance HasModifiersFor ScientistOfYith where
  getModifiersFor target (ScientistOfYith a) | isTarget a target = do
    activatedTheDevice <- remembered ActivatedTheDevice
    dissectedAnOrgan <- remembered DissectedAnOrgan
    pure
      $ if activatedTheDevice || dissectedAnOrgan
        then
          toModifiers
            a
            [RemoveKeyword Keyword.Aloof, AddKeyword Keyword.Hunter]
        else []
  getModifiersFor _ _ = pure []

instance RunMessage ScientistOfYith where
  runMessage msg (ScientistOfYith attrs) =
    ScientistOfYith <$> runMessage msg attrs
