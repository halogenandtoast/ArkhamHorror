{-# LANGUAGE AllowAmbiguousTypes #-}

{- | Ultimatums contributed by homebrew campaigns.

A campaign declares its own in a leaf @UltimatumDefs.hs@ — the enum and nothing
else — so that core 'Arkham.UltimatumsAndBoons.Types' can read the list for the
catalog without importing campaign code. The behavior lives wherever the
campaign already handles the rule it is bending, gated on
'Arkham.UltimatumsAndBoons.hasUltimatumOrBoon'.

The wire name is @":\<campaign-id\>:\<Key\>"@, which is how the frontend knows
which campaign an ultimatum belongs to and offers it only there.
-}
module Arkham.Homebrew.UltimatumDefs where

import Arkham.Prelude

data HomebrewUltimatumDef = HomebrewUltimatumDef
  { huCampaign :: Text
  -- ^ Set by 'campaignUltimatums'; the campaign id, e.g. @":dark-matter"@.
  , huKey :: Text
  -- ^ The ultimatum's own name, unique within the campaign.
  }

-- | The wire name, and the tag the open 'Ultimatum' constructor carries.
huWireName :: HomebrewUltimatumDef -> Text
huWireName d = huCampaign d <> ":" <> huKey d

{- | Stamp a campaign id onto its ultimatums, in printed order:

> ultimatums = campaignUltimatums ":dark-matter" [ultimatum "UltimatumOfImpendingDoom", ...]
-}
campaignUltimatums :: Text -> [HomebrewUltimatumDef] -> [HomebrewUltimatumDef]
campaignUltimatums cid = map \d -> d {huCampaign = cid}

ultimatum :: Text -> HomebrewUltimatumDef
ultimatum = HomebrewUltimatumDef ""

{- | Implement in your campaign's @UltimatumDefs.hs@ on a campaign-local tag
type; the instance is discovered automatically (see
'Arkham.Homebrew.Ultimatums').
-}
class IsHomebrewUltimatums a where
  homebrewUltimatums :: [HomebrewUltimatumDef]
