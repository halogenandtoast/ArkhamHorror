{- | The Masque of the Red Death is a one-scenario side story, so this is its
whole side-story list: what it costs to add to a campaign in progress.

A leaf module by design: 'Arkham.SideStory' reads the costs, so it must not
import engine code. The scenario itself is registered in
"Arkham.Homebrew.TheMasqueOfTheRedDeath.Content".
-}
module Arkham.Homebrew.TheMasqueOfTheRedDeath.SideStoryDefs where

import Arkham.Homebrew.SideStoryDefs

data TheMasqueOfTheRedDeathSideStories

instance IsHomebrewSideStories TheMasqueOfTheRedDeathSideStories where
  homebrewSideStories = [sideStory ":the-masque-of-the-red-death:001" 2]
