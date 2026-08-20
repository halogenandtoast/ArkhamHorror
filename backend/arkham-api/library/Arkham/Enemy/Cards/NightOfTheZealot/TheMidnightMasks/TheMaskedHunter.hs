module Arkham.Enemy.Cards.NightOfTheZealot.TheMidnightMasks.TheMaskedHunter (theMaskedHunter) where

import Arkham.Classes
import Arkham.Enemy.CardDefs.NightOfTheZealot.TheMidnightMasks qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Prelude

newtype TheMaskedHunter = TheMaskedHunter EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theMaskedHunter :: EnemyCard TheMaskedHunter
theMaskedHunter =
  enemyWith TheMaskedHunter Cards.theMaskedHunter
    $ preyL
    .~ Prey MostClues

instance HasModifiersFor TheMaskedHunter where
  getModifiersFor (TheMaskedHunter a) = do
    healthModifier <- perPlayer 2
    modifySelf a [HealthModifier healthModifier]
    modifySelect a (investigatorEngagedWith a) [CannotDiscoverClues, CannotSpendClues]

instance RunMessage TheMaskedHunter where
  runMessage msg (TheMaskedHunter attrs) = TheMaskedHunter <$> runMessage msg attrs
