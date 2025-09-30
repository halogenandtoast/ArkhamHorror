module Arkham.Enemy.Cards.WitnessOfChaos (witnessOfChaos) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers

newtype WitnessOfChaos = WitnessOfChaos EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witnessOfChaos :: EnemyCard WitnessOfChaos
witnessOfChaos =
  enemy WitnessOfChaos Cards.witnessOfChaos (4, Static 4, 2) (1, 1)
    & setSpawnAt FewestBreaches

instance HasAbilities WitnessOfChaos where
  getAbilities (WitnessOfChaos a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyEnters #when Anywhere (be a)

instance RunMessage WitnessOfChaos where
  runMessage msg e@(WitnessOfChaos attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withLocationOf attrs (`placeBreaches` 1)
      pure e
    _ -> WitnessOfChaos <$> liftRunMessage msg attrs
