module Arkham.Enemy.Cards.TheDunwichLegacy.TheEssexCountyExpress.EmergentMonstrosity (emergentMonstrosity) where

import Arkham.Direction
import Arkham.Enemy.CardDefs.TheDunwichLegacy.TheEssexCountyExpress qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype EmergentMonstrosity = EmergentMonstrosity EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

emergentMonstrosity :: EnemyCard EmergentMonstrosity
emergentMonstrosity =
  enemy EmergentMonstrosity Cards.emergentMonstrosity
    & setSpawnAt (firstOf [LocationInDirection RightOf YourLocation, YourLocation])
    & setExhausted

instance RunMessage EmergentMonstrosity where
  runMessage msg (EmergentMonstrosity attrs) =
    EmergentMonstrosity <$> runMessage msg attrs
