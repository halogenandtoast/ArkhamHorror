module Arkham.Enemy.Cards.Heretic_A (
  heretic_A,
  Heretic_A (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Keyword (Keyword (Aloof))
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (Spectral))

newtype Heretic_A = Heretic_A EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heretic_A :: EnemyCard Heretic_A
heretic_A = enemy Heretic_A Cards.heretic_A (4, Static 2, 3) (1, 1)

instance HasModifiersFor Heretic_A where
  getModifiersFor target (Heretic_A a) | isTarget a target = do
    n <- perPlayer 2
    atNonSpectralLocation <-
      selectAny $ locationWithEnemy (enemyId a) <> NotLocation (LocationWithTrait Spectral)
    pure $
      toModifiers a $
        HealthModifier n
          : if atNonSpectralLocation then [AddKeyword Aloof, CannotBeDamaged, CannotBeEngaged] else []
  getModifiersFor _ _ = pure []

instance HasAbilities Heretic_A where
  getAbilities (Heretic_A a) =
    withBaseAbilities
      a
      [ restrictedAbility a 1 OnSameLocation $ FastAbility $ ClueCost 1
      , mkAbility a 2 $ ForcedAbility $ EnemyDefeated Timing.After Anyone $ EnemyWithId $ toId a
      ]

instance RunMessage Heretic_A where
  runMessage msg (Heretic_A attrs) =
    Heretic_A <$> runMessage msg attrs
