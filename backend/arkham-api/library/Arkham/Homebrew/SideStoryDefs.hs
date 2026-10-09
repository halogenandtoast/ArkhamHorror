{-# LANGUAGE AllowAmbiguousTypes #-}

{- | Side stories contributed by homebrew campaigns.

A campaign declares its list in its own @SideStoryDefs.hs@ — a leaf module
holding nothing else, so that 'Arkham.SideStory' can read the costs without
importing any campaign code.

The scenario itself is registered the usual way, in the campaign's
@Content.hs@; this only says what it costs to add it to a campaign in progress,
and is what makes it a side story rather than a standalone-only scenario.
-}
module Arkham.Homebrew.SideStoryDefs where

import Arkham.Id
import Arkham.Prelude

data HomebrewSideStoryDef = HomebrewSideStoryDef
  { hsScenario :: ScenarioId
  , hsCost :: Int
  -- ^ The xp each investigator spends to add it, before any campaign overlay.
  }

-- | > sideStory ":the-symphony-of-erich-zann:001" 2
sideStory :: ScenarioId -> Int -> HomebrewSideStoryDef
sideStory = HomebrewSideStoryDef

{- | Implement in your campaign's @SideStoryDefs.hs@ on a campaign-local tag
type; the instance is discovered automatically (see
'Arkham.Homebrew.SideStories').
-}
class IsHomebrewSideStories a where
  homebrewSideStories :: [HomebrewSideStoryDef]
