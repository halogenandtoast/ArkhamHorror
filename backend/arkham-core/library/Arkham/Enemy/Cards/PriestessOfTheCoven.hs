module Arkham.Enemy.Cards.PriestessOfTheCoven
  ( priestessOfTheCoven
  , PriestessOfTheCoven(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Classes
import Arkham.Enemy.Runner

newtype PriestessOfTheCoven = PriestessOfTheCoven EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

priestessOfTheCoven :: EnemyCard PriestessOfTheCoven
priestessOfTheCoven = enemy PriestessOfTheCoven Cards.priestessOfTheCoven (2, Static 3, 2) (2, 0)

instance RunMessage PriestessOfTheCoven where
  runMessage msg (PriestessOfTheCoven attrs) =
    PriestessOfTheCoven <$> runMessage msg attrs
