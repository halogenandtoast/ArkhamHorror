{-# LANGUAGE TemplateHaskell #-}

{- | Side story lists contributed by homebrew campaigns. Campaigns are
discovered: any @Arkham/Homebrew/\<Name\>/SideStoryDefs.hs@ with an
'IsHomebrewSideStories' instance is folded in automatically — no edits here
when adding a campaign.
-}
module Arkham.Homebrew.SideStories where

import Arkham.Homebrew.SideStoryDefs
import Arkham.Homebrew.SideStoryEntries (DiscoveredModules)
import Arkham.Homebrew.TH
import Arkham.Id
import Arkham.Prelude

allHomebrewSideStories :: [HomebrewSideStoryDef]
allHomebrewSideStories = $(discoverInstances ''IsHomebrewSideStories 'homebrewSideStories)

{- | Unused; see 'discoveredModules'. Without it GHC does not rebuild this
module when a campaign is added.
-}
discoveredSideStoryModules :: Text
discoveredSideStoryModules = discoveredModules @DiscoveredModules

homebrewSideStoryCosts :: [(ScenarioId, Int)]
homebrewSideStoryCosts = [(hsScenario d, hsCost d) | d <- allHomebrewSideStories]
