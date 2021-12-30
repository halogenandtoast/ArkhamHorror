module Arkham.TokenStrategy where

import ClassyPrelude

import Arkham.Json

data TokenStrategy = DefaultTokenStrategy | DrawXResolveOne Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
