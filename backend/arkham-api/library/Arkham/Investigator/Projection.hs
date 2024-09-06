{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Investigator.Projection (
  module X,
) where

import Arkham.Card
import {-# SOURCE #-} Arkham.Game ()
import Arkham.GameT
import Arkham.Id
import Arkham.Investigator.Types as X (Field (..))
import Arkham.Message
import Arkham.Projection
import Arkham.Queue
import GHC.Records

instance HasField "hand" InvestigatorId (QueueT Message GameT [Card]) where
  getField = field InvestigatorHand
