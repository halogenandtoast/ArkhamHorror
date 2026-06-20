module Arkham.Enemy.Cards.EixodolonsPet (eixodolonsPet, isLockedAway) where

import Arkham.Classes.HasGame
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (..))
import Arkham.Placement
import Arkham.Projection
import Arkham.Tracing

newtype EixodolonsPet = EixodolonsPet EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- While Eixodolon's Pet is "locked away" (in play near the Chamber of
-- Hunger, but not at any location), it is immune to player card effects.
-- Being out of any location keeps player cards from reaching it; only the
-- Hunger Diagram can hurt it.
eixodolonsPet :: EnemyCard EixodolonsPet
eixodolonsPet = enemy EixodolonsPet Cards.eixodolonsPet

isLockedAway :: (HasGame m, Tracing m) => EnemyId -> m Bool
isLockedAway eid =
  fieldMap EnemyPlacement (== Global) eid

instance RunMessage EixodolonsPet where
  runMessage msg (EixodolonsPet attrs) = EixodolonsPet <$> runMessage msg attrs
