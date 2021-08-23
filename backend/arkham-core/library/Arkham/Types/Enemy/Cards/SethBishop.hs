module Arkham.Types.Enemy.Cards.SethBishop
  ( sethBishop
  , SethBishop(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs

newtype SethBishop = SethBishop EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities env)

sethBishop :: EnemyCard SethBishop
sethBishop = enemy SethBishop Cards.sethBishop (5, PerPlayer 3, 5) (1, 1)

instance EnemyAttrsRunMessage env => RunMessage env SethBishop where
  runMessage msg (SethBishop attrs) = SethBishop <$> runMessage msg attrs
