module Arkham.Enemy.Cards.WitnessOfChaos (
  witnessOfChaos,
  WitnessOfChaos (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype WitnessOfChaos = WitnessOfChaos EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

witnessOfChaos :: EnemyCard WitnessOfChaos
witnessOfChaos =
  enemyWith
    WitnessOfChaos
    Cards.witnessOfChaos
    (4, Static 4, 2)
    (1, 1)
    (spawnAtL ?~ SpawnAt FewestBreaches)

instance HasAbilities WitnessOfChaos where
  getAbilities (WitnessOfChaos attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1
          $ ForcedAbility
          $ EnemyEnters Timing.When Anywhere
          $ EnemyWithId (toId attrs)
      ]

instance RunMessage WitnessOfChaos where
  runMessage msg e@(WitnessOfChaos attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      location <- fieldJust EnemyLocation (toId attrs)
      push $ PlaceBreaches (toTarget location) 1
      pure e
    _ -> WitnessOfChaos <$> runMessage msg attrs
