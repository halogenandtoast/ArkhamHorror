module Arkham.Enemy.Cards.CantorOfFlame (cantorOfFlame) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CantorOfFlame = CantorOfFlame EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

cantorOfFlame :: EnemyCard CantorOfFlame
cantorOfFlame = enemy CantorOfFlame Cards.cantorOfFlame (2, Static 2, 2) (1, 0)
