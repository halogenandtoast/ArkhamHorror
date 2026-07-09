{- | Engine-side achievement helpers. Detection code calls 'earnAchievement';
the API layer (Api.Handler.Arkham.Games.Shared) observes the resulting
'EarnAchievement' messages, persists a row per human player, and pushes the
unlock toast. The engine itself never touches the database.
-}
module Arkham.Achievement (
  module Arkham.Achievement,
  module Arkham.Achievement.Types,
) where

import Arkham.Achievement.Types
import Arkham.Campaign.Types (Campaign, campaignId)
import Arkham.Classes.Entity (toAttrs)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Game.Base
import Arkham.Game.Settings (settingsAchievementsEnabled)
import Arkham.Id (unCampaignId)
import Arkham.Message
import Arkham.Prelude

-- Inlined rather than Arkham.Game.Utils.modeCampaign: campaign modules import
-- this module, and Game.Utils pulls Entities -> Campaign.Campaigns (a cycle).
currentCampaign :: GameMode -> Maybe Campaign
currentCampaign = \case
  This c -> Just c
  These c _ -> Just c
  That _ -> Nothing

-- | Campaign id of the game in play, if any.
currentCampaignId :: HasGame m => m (Maybe Text)
currentCampaignId =
  fmap (unCampaignId . campaignId . toAttrs) . currentCampaign . gameMode <$> getGame

{- | Push the earn if achievements are enabled for this game and the current
campaign matches the achievement's list (e.g. Return to NOTZ only). The API
layer additionally dedupes against already-earned rows, so re-fires are safe.
-}
earnAchievement :: (HasGame m, HasQueue Message m) => Achievement -> m ()
earnAchievement achievement = do
  enabled <- settingsAchievementsEnabled . gameSettings <$> getGame
  mCampaignId <- currentCampaignId
  -- Priority: earns often coincide with act advances / resolutions whose
  -- handlers clearQueue; a priority message is popped next regardless of
  -- queue position, so it is consumed before any clear can eat it.
  when (enabled && maybe False (`elem` achievementCampaigns achievement) mCampaignId)
    $ push
    $ Priority
    $ EarnAchievement achievement
