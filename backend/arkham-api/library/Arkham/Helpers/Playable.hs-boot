module Arkham.Helpers.Playable where

import Arkham.Card (Card)
import Arkham.Classes.HasGame (HasGame)
import Arkham.Cost.Status (CostStatus)
import Arkham.Id
import Arkham.Investigator.Types (InvestigatorAttrs)
import Arkham.Prelude
import Arkham.Source
import Arkham.Window (Window)

getIsPlayable
  :: (HasCallStack, HasGame m, Sourceable source)
  => InvestigatorId
  -> source
  -> CostStatus
  -> [Window]
  -> Card
  -> m Bool
getPlayableCards
  :: (HasCallStack, HasGame m) => InvestigatorAttrs -> CostStatus -> [Window] -> m [Card]

getIsPlayableWithResources
  :: forall m source
   . (HasCallStack, HasGame m, Sourceable source)
  => InvestigatorId
  -> source
  -> Int
  -> CostStatus
  -> [Window]
  -> Card
  -> m Bool
