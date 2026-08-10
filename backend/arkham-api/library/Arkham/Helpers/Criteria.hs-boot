module Arkham.Helpers.Criteria where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Cost
import Arkham.Criteria (Criterion)
import Arkham.Id
import Arkham.Prelude
import Arkham.Source
import Arkham.Window (Window)

passesCriteria
  :: (HasCallStack, HasGame m)
  => InvestigatorId
  -> Maybe (Card, CostStatus)
  -> Source
  -> Source
  -> [Window]
  -> Criterion
  -> m Bool
