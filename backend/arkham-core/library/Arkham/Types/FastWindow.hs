module Arkham.Types.FastWindow where

import ClassyPrelude
import Data.Aeson

data Where = YourLocation
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data Who = You | InvestigatorAtYourLocation
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data FastWindow = WhenEnemyAttacks Who | WhenEnemyDefeated Who | DuringTurn Who | WhenDiscoverClues Who Where
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

