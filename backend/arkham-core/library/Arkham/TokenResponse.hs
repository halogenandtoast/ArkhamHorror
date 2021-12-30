module Arkham.TokenResponse
  ( TokenResponse(..)
  ) where

import Arkham.Prelude

import Arkham.Token

data TokenResponse a = OnAnyToken [Token] [a]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
