{-# LANGUAGE TemplateHaskell #-}

{- | Ultimatums contributed by homebrew campaigns. Campaigns are discovered: any
@Arkham/Homebrew/\<Name\>/UltimatumDefs.hs@ with an 'IsHomebrewUltimatums'
instance is folded in automatically — no edits here when adding a campaign.
-}
module Arkham.Homebrew.Ultimatums where

import Arkham.Homebrew.TH
import Arkham.Homebrew.UltimatumDefs
import Arkham.Homebrew.UltimatumEntries (DiscoveredModules)
import Arkham.Prelude

allHomebrewUltimatums :: [HomebrewUltimatumDef]
allHomebrewUltimatums = $(discoverInstances ''IsHomebrewUltimatums 'homebrewUltimatums)

{- | Unused; see 'discoveredModules'. Without it GHC does not rebuild this
module when a campaign is added.
-}
discoveredUltimatumModules :: Text
discoveredUltimatumModules = discoveredModules @DiscoveredModules

-- | Every homebrew ultimatum's wire name, in each campaign's printed order.
homebrewUltimatumNames :: [Text]
homebrewUltimatumNames = map huWireName allHomebrewUltimatums
