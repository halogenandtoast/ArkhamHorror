{- | Against the Wendigo is a one-scenario side story, so this is its whole
side-story list: what it costs to add to a campaign in progress.

A leaf module by design: 'Arkham.SideStory' reads the costs, so it must not
import engine code. The scenario itself is registered in
"Arkham.Homebrew.AgainstTheWendigo.Content".
-}
module Arkham.Homebrew.AgainstTheWendigo.SideStoryDefs where

import Arkham.Homebrew.SideStoryDefs

data AgainstTheWendigoSideStories

instance IsHomebrewSideStories AgainstTheWendigoSideStories where
  homebrewSideStories = [sideStory ":against-the-wendigo:001" 2]
