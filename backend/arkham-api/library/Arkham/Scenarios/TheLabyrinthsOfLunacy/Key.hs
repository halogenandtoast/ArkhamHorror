module Arkham.Scenarios.TheLabyrinthsOfLunacy.Key where

import Arkham.Prelude

-- | The fate of a single group during the mini-campaign.
data GroupOutcome
  = TheGroupEscapedTheLabyrinth
  | TheGroupPerished
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

-- | Campaign-log sections, one per group (A/B/C). Each wraps the group's
-- outcome so the log renders a section per group (see the frontend's section
-- detection in CampaignLog.vue, which keys off the nested @{tag, contents}@ shape).
data TheLabyrinthsOfLunacyKey
  = GroupA GroupOutcome
  | GroupB GroupOutcome
  | GroupC GroupOutcome
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)
