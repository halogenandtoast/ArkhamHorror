{-# LANGUAGE TemplateHaskell #-}

{- | Achievement lists contributed by homebrew campaigns. Campaigns are
discovered: any @Arkham/Homebrew/\<Name\>/AchievementDefs.hs@ with an
'IsHomebrewAchievements' instance is folded in automatically — no edits here
when adding a campaign.
-}
module Arkham.Homebrew.Achievements where

import Arkham.Homebrew.AchievementDefs
import Arkham.Homebrew.AchievementEntries ()
import Arkham.Homebrew.TH
import Arkham.Prelude

allHomebrewAchievements :: [HomebrewAchievementDef]
allHomebrewAchievements = $(discoverInstances ''IsHomebrewAchievements 'homebrewAchievements)

-- | Every homebrew achievement's wire name, in each campaign's printed order.
homebrewAchievementNames :: [Text]
homebrewAchievementNames = map haWireName allHomebrewAchievements

-- | Checklist items by wire name, for the achievements that have them.
homebrewAchievementChecklists :: Map Text [Text]
homebrewAchievementChecklists =
  mapFromList
    [(haWireName d, haChecklist d) | d <- allHomebrewAchievements, notNull (haChecklist d)]
