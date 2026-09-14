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
import Arkham.Id (InvestigatorId, unCampaignId)
import Arkham.Message
import Arkham.Prelude
import Arkham.Target
import Data.Aeson.Key qualified as Key

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

{- | Earn an achievement for a single investigator's player only. Same gating
and Priority rationale as 'earnAchievement'; the API layer resolves the
investigator to its player row and inserts only that one.
-}
earnAchievementBy
  :: (HasGame m, HasQueue Message m) => InvestigatorId -> Achievement -> m ()
earnAchievementBy iid achievement = do
  enabled <- settingsAchievementsEnabled . gameSettings <$> getGame
  mCampaignId <- currentCampaignId
  when (enabled && maybe False (`elem` achievementCampaigns achievement) mCampaignId)
    $ push
    $ Priority
    $ EarnAchievementBy iid achievement

{- | Report completed checklist items (see 'achievementChecklist') for a
cross-playthrough achievement. Same gating and Priority rationale as
'earnAchievement'; the API layer merges the items into the per-user
progress row and awards the earn once the checklist is complete.
-}

-- | Report checklist items for a single investigator's player only.
achievementProgressBy
  :: (HasGame m, HasQueue Message m) => InvestigatorId -> Achievement -> [Text] -> m ()
achievementProgressBy iid achievement items = do
  enabled <- settingsAchievementsEnabled . gameSettings <$> getGame
  mCampaignId <- currentCampaignId
  when (enabled && notNull items && maybe False (`elem` achievementCampaigns achievement) mCampaignId)
    $ push
    $ Priority
    $ AchievementProgressBy iid achievement items

achievementProgress :: (HasGame m, HasQueue Message m) => Achievement -> [Text] -> m ()
achievementProgress achievement items = do
  enabled <- settingsAchievementsEnabled . gameSettings <$> getGame
  mCampaignId <- currentCampaignId
  when (enabled && notNull items && maybe False (`elem` achievementCampaigns achievement) mCampaignId)
    $ push
    $ Priority
    $ AchievementProgress achievement items

{- | Bump a campaign-store counter and re-check it once the bump has landed.

A read-modify-write through the queue (@storedInt k >>= setStore k . (+1)@)
loses increments inside a 'Simultaneously' block: every branch runs with a
cleared queue against the same game state, so each branch reads the same
pre-bump value and writes the same result. 'IncrementGlobal' instead does the
arithmetic when the message is processed, and the store is game state, so the
bumps thread through the branches like any other state change.

The threshold check has to move with it: at push time the counter is still
stale. The trailing 'Do' is where it belongs -- match it with 'CounterBumped'.
-}
bumpCounter :: HasQueue Message m => Text -> Int -> m ()
bumpCounter k n = pushAll [Priority msg, Priority (Do msg)]
 where
  msg = IncrementGlobal CampaignTarget (Key.fromText k) n

{- | List-valued counterpart of 'bumpCounter': insert into a set-like list
global, most-recent first. Match the follow-up with 'GlobalInserted'.
-}
insertGlobal :: (HasQueue Message m, ToJSON a) => Text -> a -> m ()
insertGlobal k v = pushAll [Priority msg, Priority (Do msg)]
 where
  msg = InsertGlobal CampaignTarget (Key.fromText k) (toJSON v)

-- | The deferred check emitted by 'bumpCounter'; the counter now reads its new value.
pattern CounterBumped :: Text -> Message
pattern CounterBumped k <- Do (IncrementGlobal CampaignTarget (Key.toText -> k) _)

-- | The deferred check emitted by 'insertGlobal'.
pattern GlobalInserted :: Text -> Message
pattern GlobalInserted k <- Do (InsertGlobal CampaignTarget (Key.toText -> k) _)
