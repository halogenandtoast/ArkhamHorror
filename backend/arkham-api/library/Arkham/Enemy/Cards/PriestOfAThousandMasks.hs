module Arkham.Enemy.Cards.PriestOfAThousandMasks (priestOfAThousandMasks, PriestOfAThousandMasks (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Keyword (Keyword (Retaliate))
import Arkham.Modifier qualified as Mod
import Arkham.Prelude
import Arkham.Scenarios.TheSearchForKadath.Helpers

newtype PriestOfAThousandMasks = PriestOfAThousandMasks EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

priestOfAThousandMasks :: EnemyCard PriestOfAThousandMasks
priestOfAThousandMasks = enemy PriestOfAThousandMasks Cards.priestOfAThousandMasks (2, Static 2, 2) (0, 1)

instance HasModifiersFor PriestOfAThousandMasks where
  getModifiersFor target (PriestOfAThousandMasks a) | a `is` target = do
    n <- getSignsOfTheGods
    pure
      $ toModifiers a
      $ (guard (n >= 2) *> [Mod.EnemyFight 1, Mod.EnemyEvade 1])
      <> [HealthModifier 2 | n >= 4]
      <> (guard (n >= 6) *> [DamageDealt 1, AddKeyword Retaliate])
  getModifiersFor _ _ = pure []

instance RunMessage PriestOfAThousandMasks where
  runMessage msg (PriestOfAThousandMasks attrs) =
    PriestOfAThousandMasks <$> runMessage msg attrs
