module Arkham.Classes.HasChaosTokenValue where

import Arkham.ChaosToken.Types
import Arkham.Classes.HasGame
import Arkham.Id
import Arkham.Prelude
import Arkham.Tracing

class HasChaosTokenValue a where
  getChaosTokenValue
    :: (HasCallStack, HasGame m, Tracing m) => InvestigatorId -> ChaosTokenFace -> a -> m ChaosTokenValue
