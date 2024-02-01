module Arkham.Enemy.Cards.ConstanceDumaine (
  constanceDumaine,
  ConstanceDumaine (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Modifier qualified as Modifier

newtype ConstanceDumaine = ConstanceDumaine EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

constanceDumaine :: EnemyCard ConstanceDumaine
constanceDumaine =
  enemy ConstanceDumaine Cards.constanceDumaine (4, Static 6, 1) (2, 0)

instance HasModifiersFor ConstanceDumaine where
  getModifiersFor (EnemyTarget eid) (ConstanceDumaine a)
    | eid == toId a =
        pure $ toModifiers a [Modifier.EnemyFight 3 | enemyExhausted a]
  getModifiersFor _ _ = pure []

instance RunMessage ConstanceDumaine where
  runMessage msg (ConstanceDumaine attrs) =
    ConstanceDumaine <$> runMessage msg attrs
