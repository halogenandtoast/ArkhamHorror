module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.SideStoryDefs where

import Arkham.Homebrew.SideStoryDefs

data TheStrangeCaseOfDrJekyllAndMrHydeSideStories

instance IsHomebrewSideStories TheStrangeCaseOfDrJekyllAndMrHydeSideStories where
  homebrewSideStories = [sideStory ":the-strange-case-of-dr-jekyll-and-mr-hyde:001" 2]
