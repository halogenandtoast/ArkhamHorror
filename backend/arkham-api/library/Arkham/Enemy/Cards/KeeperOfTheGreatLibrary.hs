module Arkham.Enemy.Cards.KeeperOfTheGreatLibrary (
  keeperOfTheGreatLibrary,
  KeeperOfTheGreatLibrary (..),
) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Keyword qualified as Keyword
import Arkham.Prelude
import Arkham.ScenarioLogKey

newtype KeeperOfTheGreatLibrary = KeeperOfTheGreatLibrary EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

keeperOfTheGreatLibrary :: EnemyCard KeeperOfTheGreatLibrary
keeperOfTheGreatLibrary =
  enemyWith KeeperOfTheGreatLibrary Cards.keeperOfTheGreatLibrary (3, Static 4, 3) (1, 1)
    $ spawnAtL
    ?~ "Great Library"

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
