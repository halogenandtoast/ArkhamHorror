{- | The Symphony of Erich Zann is a one-scenario side story, so this is its
whole side-story list: what it costs to add to a campaign in progress.

A leaf module by design: 'Arkham.SideStory' reads the costs, so it must not
import engine code. The scenario itself is registered in
"Arkham.Homebrew.TheSymphonyOfErichZann.Content".
-}
module Arkham.Homebrew.TheSymphonyOfErichZann.SideStoryDefs where

import Arkham.Homebrew.SideStoryDefs

data TheSymphonyOfErichZannSideStories

instance IsHomebrewSideStories TheSymphonyOfErichZannSideStories where
  homebrewSideStories = [sideStory ":the-symphony-of-erich-zann:001" 2]
