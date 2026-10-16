module Arkham.Homebrew.TheInvisibleMan.SideStoryDefs where

import Arkham.Homebrew.SideStoryDefs

data TheInvisibleManSideStories

instance IsHomebrewSideStories TheInvisibleManSideStories where
  homebrewSideStories = [sideStory ":the-invisible-man:001" 2]
