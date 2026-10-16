module Arkham.Homebrew.TheLegendOfSleepyHollow.SideStoryDefs where

import Arkham.Homebrew.SideStoryDefs

data TheLegendOfSleepyHollowSideStories

instance IsHomebrewSideStories TheLegendOfSleepyHollowSideStories where
  homebrewSideStories = [sideStory ":the-legend-of-sleepy-hollow:001" 2]
