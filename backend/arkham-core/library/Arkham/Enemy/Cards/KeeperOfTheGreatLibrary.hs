module Arkham.Enemy.Cards.KeeperOfTheGreatLibrary (
  keeperOfTheGreatLibrary,
  KeeperOfTheGreatLibrary (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.ScenarioLogKey

newtype KeeperOfTheGreatLibrary = KeeperOfTheGreatLibrary EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

keeperOfTheGreatLibrary :: EnemyCard KeeperOfTheGreatLibrary
keeperOfTheGreatLibrary =
  enemyWith
    KeeperOfTheGreatLibrary
    Cards.keeperOfTheGreatLibrary
    (3, Static 4, 3)
    (1, 1)
    (spawnAtL ?~ "Great Library")

instance HasModifiersFor KeeperOfTheGreatLibrary where
  getModifiersFor target (KeeperOfTheGreatLibrary a) | isTarget a target = do
    foundTheProcess <- remembered FoundTheProcess
    realizedWhatYearItIs <- remembered RealizedWhatYearItIs
    pure
      $ if foundTheProcess || realizedWhatYearItIs
        then
          toModifiers
            a
            [RemoveKeyword Keyword.Aloof, AddKeyword Keyword.Hunter]
        else []
  getModifiersFor _ _ = pure []

instance RunMessage KeeperOfTheGreatLibrary where
  runMessage msg (KeeperOfTheGreatLibrary attrs) =
    KeeperOfTheGreatLibrary <$> runMessage msg attrs
