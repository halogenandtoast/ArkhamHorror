module Arkham.Types.RequestedTokenStrategy
  ( RequestedTokenStrategy(..)
  )
where

import ClassyPrelude

import Arkham.Json

data RequestedTokenStrategy = SetAside | RemoveTokens
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
