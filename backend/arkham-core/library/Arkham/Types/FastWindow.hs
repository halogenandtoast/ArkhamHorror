module Arkham.Types.FastWindow where

import Arkham.Types.SkillType
import ClassyPrelude
import Data.Aeson

data Where = YourLocation
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data Who = You | InvestigatorAtYourLocation
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data FastWindow = WhenDrawTreachery Who Bool | WhenEnemyAttacks Who | WhenEnemyDefeated Who | DuringTurn Who | WhenDiscoverClues Who Where | WhenSkillTest SkillType | Any
  --                                    ^ is weakness
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

