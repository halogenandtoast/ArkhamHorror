module Arkham.Enemy.Cards.EixodolonsPet (eixodolonsPet, isLockedAway) where

import Arkham.Classes.HasGame
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Modifiers (immuneToPlayerEffects)
import Arkham.Placement
import Arkham.Projection
import Arkham.Tracing

newtype EixodolonsPet = EixodolonsPet EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- While Eixodolon's Pet is "locked away" (in play near the Chamber of
-- Hunger, but not at any location), it is immune to player card effects.
-- The Hunger Diagram is an encounter card effect, so it can still damage the
-- Pet while player card effects cannot.
eixodolonsPet :: EnemyCard EixodolonsPet
eixodolonsPet = enemy EixodolonsPet Cards.eixodolonsPet

isLockedAway :: (HasGame m, Tracing m) => EnemyId -> m Bool
isLockedAway eid =
  fieldMap EnemyPlacement (== Global) eid

instance HasModifiersFor EixodolonsPet where
  getModifiersFor (EixodolonsPet a) =
    when (a.placement == Global) $ immuneToPlayerEffects a

instance RunMessage EixodolonsPet where
  runMessage msg (EixodolonsPet attrs) = EixodolonsPet <$> runMessage msg attrs
