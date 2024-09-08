{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Location.Projection (
  module X,
) where

import {-# SOURCE #-} Arkham.Game ()
import Arkham.GameT
import Arkham.Id
import Arkham.Location.Types as X (Field (..))
import Arkham.Message
import Arkham.Prelude
import Arkham.Projection
import Arkham.Queue
import GHC.Records

instance HasField "shroud" LocationId (QueueT Message GameT (Maybe Int)) where
  getField = field LocationShroud
