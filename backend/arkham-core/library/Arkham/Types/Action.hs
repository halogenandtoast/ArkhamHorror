module Arkham.Types.Action where

import ClassyPrelude
import Data.Aeson

data ActionType
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

allActionTypes :: [ActionType]
allActionTypes = [minBound .. maxBound]
