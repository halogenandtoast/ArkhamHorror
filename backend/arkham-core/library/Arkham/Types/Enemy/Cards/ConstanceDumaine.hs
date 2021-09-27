module Arkham.Types.Enemy.Cards.ConstanceDumaine
  ( constanceDumaine
  , ConstanceDumaine(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype ConstanceDumaine = ConstanceDumaine EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

constanceDumaine :: EnemyCard ConstanceDumaine
constanceDumaine =
  enemy ConstanceDumaine Cards.constanceDumaine (4, Static 6, 1) (2, 0)

instance HasModifiersFor env ConstanceDumaine where
  getModifiersFor _ (EnemyTarget eid) (ConstanceDumaine a) | eid == toId a =
    pure $ toModifiers a [ EnemyFight 3 | enemyExhausted a ]
  getModifiersFor _ _ _ = pure []

instance EnemyRunner env => RunMessage env ConstanceDumaine where
  runMessage msg (ConstanceDumaine attrs) =
    ConstanceDumaine <$> runMessage msg attrs
