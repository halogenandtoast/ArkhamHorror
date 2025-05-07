module Arkham.Enemy.Cards.EmergentMonstrosity (emergentMonstrosity) where

import Arkham.Direction
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype EmergentMonstrosity = EmergentMonstrosity EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

emergentMonstrosity :: EnemyCard EmergentMonstrosity
emergentMonstrosity =
  enemy EmergentMonstrosity Cards.emergentMonstrosity (4, Static 5, 3) (2, 2)
    & setSpawnAt (firstOf [LocationInDirection RightOf YourLocation, YourLocation])
    & setExhausted

instance RunMessage EmergentMonstrosity where
  runMessage msg (EmergentMonstrosity attrs) =
    EmergentMonstrosity <$> runMessage msg attrs
