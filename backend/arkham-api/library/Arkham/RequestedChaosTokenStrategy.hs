module Arkham.RequestedChaosTokenStrategy (
  RequestedChaosTokenStrategy (..),
) where

import Arkham.Prelude

data RequestedChaosTokenStrategy = SetAside | RemoveChaosTokens
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)
