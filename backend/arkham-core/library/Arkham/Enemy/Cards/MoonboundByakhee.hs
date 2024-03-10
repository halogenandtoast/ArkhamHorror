module Arkham.Enemy.Cards.MoonboundByakhee (
  moonboundByakhee,
  MoonboundByakhee (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype MoonboundByakhee = MoonboundByakhee EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

moonboundByakhee :: EnemyCard MoonboundByakhee
moonboundByakhee = enemy MoonboundByakhee Cards.moonboundByakhee (0, Static 1, 0) (0, 0)

instance RunMessage MoonboundByakhee where
  runMessage msg (MoonboundByakhee attrs) =
    MoonboundByakhee <$> runMessage msg attrs
