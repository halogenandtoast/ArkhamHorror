module Arkham.Campaigns.TheCircleUndone.Memento.Helpers where

import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.Campaigns.TheCircleUndone.Memento
import Arkham.Classes.HasGame
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Log
import Arkham.Prelude

discoverMemento :: ReverseQueue m => Memento -> m ()
discoverMemento memento = recordSetInsert MementosDiscovered [memento]

getMementosDiscoveredCount :: HasGame m => m Int
getMementosDiscoveredCount = length <$> getRecordSet MementosDiscovered
