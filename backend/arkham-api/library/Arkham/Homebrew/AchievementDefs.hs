{-# LANGUAGE AllowAmbiguousTypes #-}

{- | Achievements contributed by homebrew campaigns.

A campaign declares its list in its own @AchievementDefs.hs@ — a leaf module
holding the campaign's achievement enum and nothing else, so that core
'Arkham.Achievement.Types' can read the list without importing any campaign
code. The detection itself lives in the campaign's @Achievements.hs@, hooked
into its own @runMessage@; nothing registers it.

The wire\/database name of a homebrew achievement is
@":\<campaign-id\>:\<Key\>"@, which is how 'Arkham.Achievement.Types' tells
which campaign it belongs to without a lookup table.
-}
module Arkham.Homebrew.AchievementDefs where

import Arkham.Prelude

data HomebrewAchievementDef = HomebrewAchievementDef
  { haCampaign :: Text
  -- ^ Set by 'campaignAchievements'; the campaign id, e.g. @":circus-ex-mortis"@.
  , haKey :: Text
  -- ^ The achievement's own name, unique within the campaign.
  , haChecklist :: [Text]
  {- ^ Item keys when this is a cross-playthrough checklist achievement (see
  'Arkham.Achievement.Types.achievementChecklist'); empty for a plain earn.
  -}
  }

-- | The wire and database name.
haWireName :: HomebrewAchievementDef -> Text
haWireName d = haCampaign d <> ":" <> haKey d

{- | Stamp a campaign id onto its achievements, in printed order:

> achievements = campaignAchievements ":circus-ex-mortis" [achievement "Scapegoat", ...]
-}
campaignAchievements :: Text -> [HomebrewAchievementDef] -> [HomebrewAchievementDef]
campaignAchievements cid = map \d -> d {haCampaign = cid}

achievement :: Text -> HomebrewAchievementDef
achievement k = HomebrewAchievementDef "" k []

-- | An achievement completed item-by-item across playthroughs.
checklistAchievement :: Text -> [Text] -> HomebrewAchievementDef
checklistAchievement = HomebrewAchievementDef ""

{- | Implement in your campaign's @AchievementDefs.hs@ on a campaign-local tag
type; the instance is discovered automatically (see
'Arkham.Homebrew.Achievements').
-}
class IsHomebrewAchievements a where
  homebrewAchievements :: [HomebrewAchievementDef]
