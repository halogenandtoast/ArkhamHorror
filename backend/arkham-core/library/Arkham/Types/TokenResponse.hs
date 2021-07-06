module Arkham.Types.TokenResponse
  ( TokenResponse(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Token

data TokenResponse a = OnAnyToken [Token] [a]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
