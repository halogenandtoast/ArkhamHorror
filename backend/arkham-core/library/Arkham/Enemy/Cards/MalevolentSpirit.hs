module Arkham.Enemy.Cards.MalevolentSpirit (
  malevolentSpirit,
  MalevolentSpirit (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Keyword (Keyword (Hunter))
import Arkham.Matcher
import Arkham.Trait (Trait (Spectral))

newtype MalevolentSpirit = MalevolentSpirit EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

malevolentSpirit :: EnemyCard MalevolentSpirit
malevolentSpirit =
  enemyWith
    MalevolentSpirit
    Cards.malevolentSpirit
    (2, Static 2, 4)
    (0, 1)
    ( spawnAtL
        ?~ SpawnLocation
          (LocationMatchAny [LocationWithTitle "Chapel Attic", LocationWithTitle "Chapel Crypt"])
    )

instance HasModifiersFor MalevolentSpirit where
  getModifiersFor target (MalevolentSpirit a) | isTarget a target = do
    atSpectral <- selectAny $ locationWithEnemy (toId a) <> LocationWithTrait Spectral
    pure $ toModifiers a $ if atSpectral then [AddKeyword Hunter, DamageDealt 1, HorrorDealt 1] else []
  getModifiersFor _ _ = pure []

instance RunMessage MalevolentSpirit where
  runMessage msg (MalevolentSpirit attrs) =
    MalevolentSpirit <$> runMessage msg attrs
