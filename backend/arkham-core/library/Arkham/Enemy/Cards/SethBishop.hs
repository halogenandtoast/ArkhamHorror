module Arkham.Enemy.Cards.SethBishop (
  sethBishop,
  SethBishop (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype SethBishop = SethBishop EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

sethBishop :: EnemyCard SethBishop
sethBishop = enemy SethBishop Cards.sethBishop (5, PerPlayer 3, 5) (1, 1)

instance RunMessage SethBishop where
  runMessage msg (SethBishop attrs) = SethBishop <$> runMessage msg attrs
