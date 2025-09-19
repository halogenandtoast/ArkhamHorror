module Arkham.Enemy.Cards.CaptiveSubjects (captiveSubjects) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Keyword
import Arkham.ScenarioLogKey

newtype CaptiveSubjects = CaptiveSubjects EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

captiveSubjects :: EnemyCard CaptiveSubjects
captiveSubjects =
  enemy CaptiveSubjects Cards.captiveSubjects (2, Static 5, 4) (0, 2)
    & setSpawnAt "Alien Conservatory"

instance HasModifiersFor CaptiveSubjects where
  getModifiersFor (CaptiveSubjects a) = do
    readAboutEarth <- remembered ReadAboutEarth
    sawAFamiliarSpecimen <- remembered SawAFamiliarSpecimen
    modifySelfWhen a (readAboutEarth || sawAFamiliarSpecimen) [RemoveKeyword Aloof, AddKeyword Hunter]

instance RunMessage CaptiveSubjects where
  runMessage msg (CaptiveSubjects attrs) = CaptiveSubjects <$> runMessage msg attrs
