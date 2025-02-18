module Arkham.Helpers.Cost where

import Arkham.Action (Action)
import Arkham.Classes.HasGame
import Arkham.Cost
import Arkham.Id
import Arkham.Source
import Arkham.Prelude
import Arkham.Window (Window)

getCanAffordCost
  :: (HasGame m, Sourceable source)
  => InvestigatorId
  -> source
  -> [Action]
  -> [Window]
  -> Cost
  -> m Bool
