module Arkham.Classes.HasTokenValue where

import Arkham.Prelude

import Arkham.Id
import Arkham.Token
import {-# SOURCE #-} Arkham.GameEnv

class HasTokenValue a where
  getTokenValue :: (HasCallStack, Monad m, HasGame m) => InvestigatorId -> TokenFace -> a -> m TokenValue
