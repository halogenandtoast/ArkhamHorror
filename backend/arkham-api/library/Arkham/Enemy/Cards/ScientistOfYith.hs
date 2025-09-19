module Arkham.Enemy.Cards.ScientistOfYith (scientistOfYith) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.Keyword qualified as Keyword
import Arkham.ScenarioLogKey

newtype ScientistOfYith = ScientistOfYith EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

scientistOfYith :: EnemyCard ScientistOfYith
scientistOfYith =
  enemy ScientistOfYith Cards.scientistOfYith (4, Static 3, 1) (2, 0)
    & setSpawnAt "Laboratory of the Great Race"

instance HasModifiersFor ScientistOfYith where
  getModifiersFor (ScientistOfYith a) = do
    activatedTheDevice <- remembered ActivatedTheDevice
    dissectedAnOrgan <- remembered DissectedAnOrgan
    modifySelfWhen
      a
      (activatedTheDevice || dissectedAnOrgan)
      [RemoveKeyword Keyword.Aloof, AddKeyword Keyword.Hunter]

instance RunMessage ScientistOfYith where
  runMessage msg (ScientistOfYith attrs) = ScientistOfYith <$> runMessage msg attrs
