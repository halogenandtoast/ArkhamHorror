module Arkham.Enemy.Cards.SilasBishop (
  SilasBishop (..),
  silasBishop,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype SilasBishop = SilasBishop EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

silasBishop :: EnemyCard SilasBishop
silasBishop = enemy SilasBishop Cards.silasBishop (3, PerPlayer 6, 7) (2, 2)

instance HasModifiersFor SilasBishop where
  getModifiersFor target (SilasBishop attrs)
    | isTarget attrs target =
        pure $ toModifiers attrs [CannotMakeAttacksOfOpportunity]
  getModifiersFor _ _ = pure []

instance RunMessage SilasBishop where
  runMessage msg (SilasBishop attrs) = SilasBishop <$> runMessage msg attrs
