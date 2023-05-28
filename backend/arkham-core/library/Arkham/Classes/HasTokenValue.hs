module Arkham.Classes.HasTokenValue where

import Arkham.Prelude

import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Store
import Arkham.Token

class HasTokenValue a where
  getTokenValue
    :: (HasCallStack, HasGame m, Store m Card) => InvestigatorId -> TokenFace -> a -> m TokenValue
