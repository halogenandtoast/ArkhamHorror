module Arkham.Helpers.Cost where

import Arkham.Action (Action)
import Arkham.Classes.HasGame
import Arkham.Cost
import Arkham.Id
import Arkham.Prelude
import Arkham.Source
import Arkham.Window (Window)

getCanAffordCost
  :: (HasGame m, Sourceable source)
  => InvestigatorId
  -> source
  -> [Action]
  -> [Window]
  -> Cost
  -> m Bool
