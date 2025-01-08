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
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

constanceDumaine :: EnemyCard ConstanceDumaine
constanceDumaine =
  enemy ConstanceDumaine Cards.constanceDumaine (4, Static 6, 1) (2, 0)

instance HasModifiersFor ConstanceDumaine where
  getModifiersFor (ConstanceDumaine a) =
    modifySelfWhen a a.exhausted [Modifier.EnemyFight 3]

instance RunMessage ConstanceDumaine where
  runMessage msg (ConstanceDumaine attrs) =
    ConstanceDumaine <$> runMessage msg attrs
