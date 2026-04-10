module Arkham.Classes.HasChaosTokenValue where

import Arkham.ChaosToken.Types
import Arkham.Classes.HasGame
import Arkham.Id
import Arkham.Prelude
import Arkham.Tracing

class HasChaosTokenValue a where
  getChaosTokenValue
    :: (HasCallStack, HasGame m, Tracing m) => InvestigatorId -> ChaosTokenFace -> a -> m ChaosTokenValue

getChaosTokenModifier
  :: (HasCallStack, HasChaosTokenValue a, HasGame m, Tracing m)
  => InvestigatorId -> ChaosTokenFace -> a -> m ChaosTokenModifier
getChaosTokenModifier iid face a =
  getChaosTokenValue iid face a <&> \case
    ChaosTokenValue _ tmod -> tmod
