module Arkham.Types.Action where

import ClassyPrelude

import Data.Aeson

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
  deriving anyclass (ToJSON, FromJSON, Hashable)
