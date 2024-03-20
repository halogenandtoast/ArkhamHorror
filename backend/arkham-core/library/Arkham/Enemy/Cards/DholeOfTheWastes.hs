module Arkham.Enemy.Cards.DholeOfTheWastes (
  dholeOfTheWastes,
  DholeOfTheWastes (..),
)
where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype DholeOfTheWastes = DholeOfTheWastes EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

dholeOfTheWastes :: EnemyCard DholeOfTheWastes
dholeOfTheWastes =
  enemyWith
    DholeOfTheWastes
    Cards.dholeOfTheWastes
    (6, Static 6, 2)
    (2, 1)
    $ (spawnAtL ?~ SpawnAt (oneOf ["Cold Wastes", "The Great Hall"]))
    . (preyL .~ Prey (InvestigatorWithLowestSkill #agility))

instance RunMessage DholeOfTheWastes where
  runMessage msg (DholeOfTheWastes attrs) =
    DholeOfTheWastes <$> runMessage msg attrs
