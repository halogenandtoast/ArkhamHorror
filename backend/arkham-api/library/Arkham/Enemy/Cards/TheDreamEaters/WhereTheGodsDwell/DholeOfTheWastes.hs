module Arkham.Enemy.Cards.TheDreamEaters.WhereTheGodsDwell.DholeOfTheWastes (
  dholeOfTheWastes,
  DholeOfTheWastes (..),
)
where

import Arkham.Classes
import Arkham.Enemy.CardDefs.TheDreamEaters.WhereTheGodsDwell qualified as Cards
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
    $ (spawnAtL ?~ SpawnAt (oneOf ["Cold Wastes", "The Great Hall"]))
    . (preyL .~ Prey (InvestigatorWithLowestSkill #agility UneliminatedInvestigator))

instance RunMessage DholeOfTheWastes where
  runMessage msg (DholeOfTheWastes attrs) =
    DholeOfTheWastes <$> runMessage msg attrs
