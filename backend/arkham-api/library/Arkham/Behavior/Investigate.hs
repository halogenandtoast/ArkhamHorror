module Arkham.Behavior.Investigate where

import Arkham.Calculation
import Arkham.Classes.HasGame (HasGame)
import Arkham.Classes.HasQueue (HasQueue)
import Arkham.GameValue
import Arkham.Helpers.Message (pushM)
import Arkham.Helpers.SkillTest.Lifted (investigate)
import Arkham.Id
import Arkham.Investigate (mkInvestigateLocation)
import Arkham.Investigate.Types qualified as I
import Arkham.Message (Message)
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Arkham.Tracing (Tracing)

{- | The 'Investigatable' behavior for things-that-can-be-investigated
(locations, enemy-locations, etc.).
-}

-- | Push an 'Investigate' for a location in response to the player using its
-- 'AbilityInvestigate' UI ability.
pushInvestigateAbility
  :: (HasGame m, HasQueue Message m, MonadRandom m, Sourceable s, Tracing m)
  => LocationId -> InvestigatorId -> s -> m ()
pushInvestigateAbility lid iid src = do
  sid <- getRandom
  pushM $ mkInvestigateLocation sid iid (toSource src) lid

-- | Resolve a non-action 'Investigate' message: set up the standard investigate
-- skill test against the given difficulty.
resolveInvestigate
  :: (ReverseQueue m, Targetable a)
  => a
  -- ^ the entity (used as the resolution target if @investigation.target@ is unset)
  -> GameCalculation
  -- ^ the investigate difficulty
  -> I.Investigate
  -> m ()
resolveInvestigate entity difficulty investigation = do
  let entityTarget = toTarget entity
  let target = maybe entityTarget (ProxyTarget entityTarget) investigation.target
  investigate
    investigation.skillTest
    investigation.investigator
    investigation.source
    target
    investigation.skillType
    difficulty

-- | Convenience: investigate at the entity's stored shroud (or 'Fixed 0' if
-- absent). Suitable for entities like enemy-locations that keep the shroud
-- right on their attrs.
resolveInvestigateAtShroud
  :: (ReverseQueue m, Targetable a)
  => a -> Maybe GameValue -> I.Investigate -> m ()
resolveInvestigateAtShroud entity shroud =
  resolveInvestigate entity (maybe (Fixed 0) GameValueCalculation shroud)
