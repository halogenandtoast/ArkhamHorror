module Arkham.RequestedChaosTokenStrategy (
  RequestedChaosTokenStrategy (..),
) where

import Arkham.Prelude

data RequestedChaosTokenStrategy = SetAside | RemoveChaosTokens
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, NoThunks, NFData)
