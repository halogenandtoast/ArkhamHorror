module Arkham.Helpers.Cost where

import Arkham.Action (Action)
import Arkham.Classes.HasGame
import Arkham.Cost
import Arkham.Id
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Arkham.Tracing
import Arkham.Window (Window)

getAdditionalActionCost :: HasGame m => InvestigatorId -> Target -> Action -> m Cost

getCanAffordAdditionalActionCost
  :: (HasCallStack, HasGame m, Tracing m, Sourceable source)
  => InvestigatorId
  -> source
  -> Target
  -> Action
  -> m Bool

getCanAffordCost
  :: (HasCallStack, HasGame m, Tracing m, Sourceable source)
  => InvestigatorId
  -> source
  -> [Action]
  -> [Window]
  -> Cost
  -> m Bool
