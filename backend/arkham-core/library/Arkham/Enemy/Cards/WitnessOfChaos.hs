module Arkham.Enemy.Cards.WitnessOfChaos
  ( witnessOfChaos
  , WitnessOfChaos(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype WitnessOfChaos = WitnessOfChaos EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

witnessOfChaos :: EnemyCard WitnessOfChaos
witnessOfChaos = enemy WitnessOfChaos Cards.witnessOfChaos (4, Static 4, 2) (1, 1)

instance RunMessage WitnessOfChaos where
  runMessage msg (WitnessOfChaos attrs) =
    WitnessOfChaos <$> runMessage msg attrs
