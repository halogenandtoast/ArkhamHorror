module Arkham.Homebrew.TheFallOfTheHouseOfUsher.SideStoryDefs where

import Arkham.Homebrew.SideStoryDefs

data TheFallOfTheHouseOfUsherSideStories

instance IsHomebrewSideStories TheFallOfTheHouseOfUsherSideStories where
  homebrewSideStories = [sideStory ":the-fall-of-the-house-of-usher:001" 2]
