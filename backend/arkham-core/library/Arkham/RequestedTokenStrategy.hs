module Arkham.RequestedTokenStrategy
  ( RequestedTokenStrategy(..)
  ) where

import Arkham.Prelude

data RequestedTokenStrategy = SetAside | RemoveTokens
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
