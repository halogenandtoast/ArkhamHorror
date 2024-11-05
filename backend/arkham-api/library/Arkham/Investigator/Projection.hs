{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Investigator.Projection (
  module X,
  module Arkham.Investigator.Projection,
) where

import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import {-# SOURCE #-} Arkham.Game ()
import Arkham.GameT
import Arkham.Id
import Arkham.Investigator.Types (InvestigatorAttrs)
import Arkham.Investigator.Types as X (Field (..))
import Arkham.Key
import Arkham.Matcher
import Arkham.Message
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Queue
import Arkham.Slot
import GHC.Records

instance HasField "hand" InvestigatorId (QueueT Message GameT [Card]) where
  getField = field InvestigatorHand

instance HasField "clues" InvestigatorId (QueueT Message GameT Int) where
  getField = field InvestigatorClues

instance HasField "placement" InvestigatorId (QueueT Message GameT Placement) where
  getField = field InvestigatorPlacement

instance HasField "hand" InvestigatorAttrs (QueueT Message GameT [Card]) where
  getField = field InvestigatorHand . toId

instance HasField "discard" InvestigatorId (QueueT Message GameT [PlayerCard]) where
  getField = field InvestigatorDiscard

getSlots :: HasGame m => SlotType -> InvestigatorId -> m [Slot]
getSlots sType iid = fieldMap InvestigatorSlots (findWithDefault [] sType) iid

instance HasField "slots" InvestigatorId (SlotType -> QueueT Message GameT [Slot]) where
  getField iid sType = getSlots sType iid

instance HasField "filter" (QueueT Message GameT [Card]) (CardMatcher -> QueueT Message GameT [Card]) where
  getField x f = filterCards f <$> x

instance HasField "keys" InvestigatorId (QueueT Message GameT (Set ArkhamKey)) where
  getField = field InvestigatorKeys

instance HasField "remainingActions" InvestigatorId (QueueT Message GameT Int) where
  getField = field InvestigatorRemainingActions
