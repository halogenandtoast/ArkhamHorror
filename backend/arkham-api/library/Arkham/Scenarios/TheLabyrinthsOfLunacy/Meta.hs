module Arkham.Scenarios.TheLabyrinthsOfLunacy.Meta where

import Arkham.Prelude

data Group = GroupA | GroupB | GroupC
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Meta = Meta
  { currentGroup :: Group
  , playedGroups :: [Group]
  -- ^ groups whose games have already been completed (mini-campaign)
  , survivedGroups :: [Group]
  -- ^ groups that escaped the labyrinth
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

initialMeta :: Group -> Meta
initialMeta g = Meta {currentGroup = g, playedGroups = [], survivedGroups = []}
