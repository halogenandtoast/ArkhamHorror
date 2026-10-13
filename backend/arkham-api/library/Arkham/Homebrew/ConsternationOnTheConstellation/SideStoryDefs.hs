{- | Consternation on the Constellation is a one-scenario side story, so this is
its whole side-story list: what it costs to add to a campaign in progress.

A leaf module by design: 'Arkham.SideStory' reads the costs, so it must not
import engine code. The scenario itself is registered in
"Arkham.Homebrew.ConsternationOnTheConstellation.Content".
-}
module Arkham.Homebrew.ConsternationOnTheConstellation.SideStoryDefs where

import Arkham.Homebrew.SideStoryDefs

data ConsternationOnTheConstellationSideStories

instance IsHomebrewSideStories ConsternationOnTheConstellationSideStories where
  homebrewSideStories = [sideStory ":consternation-on-the-constellation:001" 2]
