module Arkham.Enemy.Cards.TheMaskedHunter (TheMaskedHunter (..), theMaskedHunter) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype TheMaskedHunter = TheMaskedHunter EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theMaskedHunter :: EnemyCard TheMaskedHunter
theMaskedHunter =
  enemyWith TheMaskedHunter Cards.theMaskedHunter (4, Static 4, 2) (2, 1)
    $ preyL
    .~ Prey MostClues

instance HasModifiersFor TheMaskedHunter where
  getModifiersFor (TheMaskedHunter a) = do
    healthModifier <- perPlayer 2
    self <- modifySelf a [HealthModifier healthModifier]
    investigators <- modifySelect a (investigatorEngagedWith a) [CannotDiscoverClues, CannotSpendClues]
    pure $ self <> investigators

instance RunMessage TheMaskedHunter where
  runMessage msg (TheMaskedHunter attrs) = TheMaskedHunter <$> runMessage msg attrs
