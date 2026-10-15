module Arkham.Homebrew.ThePhantomOfTheOpera.SideStoryDefs where

import Arkham.Homebrew.SideStoryDefs

data ThePhantomOfTheOperaSideStories

instance IsHomebrewSideStories ThePhantomOfTheOperaSideStories where
  homebrewSideStories = [sideStory ":the-phantom-of-the-opera:001" 2]
