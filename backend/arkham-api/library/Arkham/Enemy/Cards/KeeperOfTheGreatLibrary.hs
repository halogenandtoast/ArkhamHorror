module Arkham.Enemy.Cards.KeeperOfTheGreatLibrary (keeperOfTheGreatLibrary) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.Keyword qualified as Keyword
import Arkham.ScenarioLogKey

newtype KeeperOfTheGreatLibrary = KeeperOfTheGreatLibrary EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

keeperOfTheGreatLibrary :: EnemyCard KeeperOfTheGreatLibrary
keeperOfTheGreatLibrary =
  enemy KeeperOfTheGreatLibrary Cards.keeperOfTheGreatLibrary (3, Static 4, 3) (1, 1)
    & setSpawnAt "Great Library"

instance HasModifiersFor KeeperOfTheGreatLibrary where
  getModifiersFor (KeeperOfTheGreatLibrary a) = do
    foundTheProcess <- remembered FoundTheProcess
    realizedWhatYearItIs <- remembered RealizedWhatYearItIs
    modifySelfWhen
      a
      (foundTheProcess || realizedWhatYearItIs)
      [RemoveKeyword Keyword.Aloof, AddKeyword Keyword.Hunter]

instance RunMessage KeeperOfTheGreatLibrary where
  runMessage msg (KeeperOfTheGreatLibrary attrs) = KeeperOfTheGreatLibrary <$> runMessage msg attrs
