module Arkham.Types.Enemy.Cards.SethBishop
  ( sethBishop
  , SethBishop(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs

newtype SethBishop = SethBishop EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sethBishop :: EnemyCard SethBishop
sethBishop = enemy SethBishop Cards.sethBishop (5, PerPlayer 3, 5) (1, 1)

instance HasModifiersFor env SethBishop

instance EnemyAttrsHasActions env => HasActions env SethBishop where
  getActions i window (SethBishop attrs) = getActions i window attrs

instance EnemyAttrsRunMessage env => RunMessage env SethBishop where
  runMessage msg (SethBishop attrs) = SethBishop <$> runMessage msg attrs
