module Arkham.Helpers.Playable where

import Arkham.Card (Card)
import Arkham.Classes.HasGame (HasGame)
import Arkham.Cost.Status (CostStatus)
import Arkham.Id
import Arkham.Prelude
import Arkham.Source
import Arkham.Window (Window)

getIsPlayable
  :: (HasCallStack, HasGame m, Sourceable source, AsId investigator, IdOf investigator ~ InvestigatorId)
  => investigator
  -> source
  -> CostStatus
  -> [Window]
  -> Card
  -> m Bool
getPlayableCards
  :: (HasCallStack, HasGame m, Sourceable source, AsId investigator, IdOf investigator ~ InvestigatorId) => source -> investigator -> CostStatus -> [Window] -> m [Card]

getIsPlayableWithResources
  :: forall m source investigator
   . (HasCallStack, HasGame m, Sourceable source, AsId investigator, IdOf investigator ~ InvestigatorId)
  => investigator
  -> source
  -> Int
  -> CostStatus
  -> [Window]
  -> Card
  -> m Bool
