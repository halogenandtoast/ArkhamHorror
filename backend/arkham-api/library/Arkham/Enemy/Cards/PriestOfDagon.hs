module Arkham.Enemy.Cards.PriestOfDagon
  ( priestOfDagon
  , PriestOfDagon(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype PriestOfDagon = PriestOfDagon EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

priestOfDagon :: EnemyCard PriestOfDagon
priestOfDagon = enemy PriestOfDagon Cards.priestOfDagon (3, Static 2, 3) (1, 1)

instance RunMessage PriestOfDagon where
  runMessage msg (PriestOfDagon attrs) =
    PriestOfDagon <$> runMessage msg attrs
