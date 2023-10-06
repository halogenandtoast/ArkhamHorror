module Arkham.Classes.HasChaosTokenValue where

import Arkham.Prelude

import Arkham.ChaosToken
import Arkham.Classes.HasGame
import Arkham.Id

class HasChaosTokenValue a where
  getChaosTokenValue
    :: (HasCallStack, HasGame m) => InvestigatorId -> ChaosTokenFace -> a -> m ChaosTokenValue
