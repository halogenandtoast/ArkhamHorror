module Arkham.Homebrew.AndThenThereWereNone.SideStoryDefs where

import Arkham.Homebrew.SideStoryDefs

data AndThenThereWereNoneSideStories

instance IsHomebrewSideStories AndThenThereWereNoneSideStories where
  homebrewSideStories = [sideStory ":and-then-there-were-none:001" 2]
