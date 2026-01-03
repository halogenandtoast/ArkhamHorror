module Arkham.Campaigns.TheFeastOfHemlockVale.Key where

import Arkham.Prelude

data TheFeastOfHemlockValeKey
  = MotherRachelRelationshipLevel
  | LeahAtwoodRelationshipLevel
  | SimeonAtwoodRelationshipLevel
  | WilliamHemlockRelationshipLevel
  | RiverHawthorneRelationshipLevel
  | GideonMizrahRelationshipLevel
  | JudithParkRelationshipLevel
  | TheoPetersRelationshipLevel
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)
