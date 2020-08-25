module Arkham.Types.TokenResponse
  ( TokenResponse(..)
  )
where

import Arkham.Json
import Arkham.Types.Token
import ClassyPrelude

data TokenResponse a = OnAnyToken [Token] [a]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
