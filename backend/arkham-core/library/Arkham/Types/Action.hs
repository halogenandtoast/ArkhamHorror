module Arkham.Types.Action where

import Arkham.Prelude

newtype TakenAction = TakenAction { unTakenAction :: Action }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Action
  = Ability
  | Draw
  | Engage
  | Evade
  | Fight
  | Investigate
  | Move
  | Parley
  | Play
  | Resign
  | Resource
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)
