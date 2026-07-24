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
  , miniCampaign :: Bool
  -- ^ whether this is a mini-campaign (all three groups) or a single standalone game
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

initialMeta :: Group -> Meta
initialMeta g =
  Meta {currentGroup = g, playedGroups = [], survivedGroups = [], miniCampaign = False}

allGroups :: [Group]
allGroups = [minBound .. maxBound]

remainingGroups :: Meta -> [Group]
remainingGroups meta = filter (`notElem` playedGroups meta) allGroups

completeCurrentGroup :: Bool -> Meta -> Meta
completeCurrentGroup survived meta =
  meta
    { playedGroups = addOnce meta.currentGroup meta.playedGroups
    , survivedGroups =
        if survived then addOnce meta.currentGroup meta.survivedGroups else meta.survivedGroups
    }
 where
  addOnce g groups
    | g `elem` groups = groups
    | otherwise = g : groups

miniCampaignComplete :: Meta -> Bool
miniCampaignComplete meta = null $ remainingGroups meta

resolutionKey :: Meta -> Text
resolutionKey meta = "resolution" <> tshow (length meta.survivedGroups + 1)
