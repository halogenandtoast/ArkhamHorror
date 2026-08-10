module Arkham.Classes.HasChaosTokenValue where

import Arkham.ChaosToken.Types
import Arkham.Classes.HasGame
import Arkham.Id
import Arkham.Prelude

class HasChaosTokenValue a where
  getChaosTokenValue
    :: (HasCallStack, HasGame m) => InvestigatorId -> ChaosTokenFace -> a -> m ChaosTokenValue