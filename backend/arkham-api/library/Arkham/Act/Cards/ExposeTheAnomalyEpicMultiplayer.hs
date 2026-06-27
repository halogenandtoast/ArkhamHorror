module Arkham.Act.Cards.ExposeTheAnomalyEpicMultiplayer (exposeTheAnomalyEpicMultiplayer) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Epic.Types (SharedKey (SharedActProgress), sharedKeyText, totalInvestigatorsKey)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Log (scenarioCount, scenarioCountIncrement)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.ScenarioLogKey (ScenarioCountKey (EpicActAdvances, EpicShared))
import Arkham.Trait (Trait (Oozified))

-- Epic Multiplayer variant of Expose the Anomaly (card 85005). In Epic
-- Multiplayer the act's clue requirement is a single GLOBAL pool shared across
-- every group in the event: 2 clues per investigator across ALL groups (the
-- event's frozen total). Each group contributes its local share (2 per local
-- player) into the shared @act-progress:1@ counter at the start of each round.
--
-- The shared pool is CUMULATIVE and is never reset (a cross-group reset would be
-- fragile). Instead each group tracks how many times THIS act has advanced in a
-- LOCAL @EpicActAdvances 1@ scenario count, and the Nth advance fires once the
-- cumulative pool reaches @2 * total * N@. Shared progress is identical in every
-- group and every group starts at 0 advances, so all groups cross each multiple
-- (2*total, 4*total, ...) at the same shared value and advance in lockstep,
-- including idle groups during the propagateShared/syncOneGroup push. This makes
-- the looping act ('ResetActDeckToStage' brings it back) genuinely require
-- another 2*total fresh clues each pass, matching single-group "re-pay" behavior.
newtype ExposeTheAnomalyEpicMultiplayer = ExposeTheAnomalyEpicMultiplayer ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exposeTheAnomalyEpicMultiplayer :: ActCard ExposeTheAnomalyEpicMultiplayer
exposeTheAnomalyEpicMultiplayer = act (1, A) ExposeTheAnomalyEpicMultiplayer Cards.exposeTheAnomalyEpicMultiplayer Nothing

-- This act's stage; both the shared progress key and the local advance-count key
-- derive from it so they can't drift.
actStage :: Int
actStage = 1

-- The shared act-progress 'SharedKey', mirrored into scenario state as
-- @EpicShared "act-progress:1"@.
actProgressKey :: SharedKey
actProgressKey = SharedActProgress actStage

-- The LOCAL (per-group, never-synced) count of how many times this act has advanced.
actAdvancesKey :: ScenarioCountKey
actAdvancesKey = EpicActAdvances actStage

instance HasAbilities ExposeTheAnomalyEpicMultiplayer where
  getAbilities (ExposeTheAnomalyEpicMultiplayer a) =
    [ -- CONTRIBUTION: the same round-start group-clue payment as the single-group
      -- act, but the paid clues feed the shared pool instead of advancing this
      -- group's act directly.
      mkAbility a 1 $ Objective $ triggered (RoundBegins #when) $ GroupClueCost (PerPlayer 2) Anywhere
    , -- AUTO-ADVANCE: an engine hook on the shared-counter mirror. propagateShared
      -- mirrors the updated pool into every group as a ScenarioCountSet (firing a
      -- ScenarioCountIncremented window); the act advances here once the cumulative
      -- pool reaches the threshold for the next advance.
      mkAbility a 2
        $ SilentForcedAbility
        $ ScenarioCountIncremented #after (EpicShared (sharedKeyText actProgressKey))
    ]

instance RunMessage ExposeTheAnomalyEpicMultiplayer where
  runMessage msg a@(ExposeTheAnomalyEpicMultiplayer attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- Contribute this group's clues (2 per local player) to the shared pool.
      n <- perPlayer 2
      push $ RaiseShared actProgressKey n
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      progress <- scenarioCount (EpicShared (sharedKeyText actProgressKey))
      total <- scenarioCount (EpicShared totalInvestigatorsKey)
      advances <- scenarioCount actAdvancesKey
      -- Cumulative threshold: the (advances + 1)th advance needs 2 * total clues
      -- beyond all prior advances.
      when (progress >= 2 * total * (advances + 1)) $ advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- Record this advance locally so the next one needs another 2 * total clues.
      scenarioCountIncrement actAdvancesKey
      locations <- select $ LocationWithTrait Oozified
      leadChooseOneM $ targets locations $ createEnemyAt_ Enemies.vulnerableHeart
      advanceActDeck attrs
      pure a
    _ -> ExposeTheAnomalyEpicMultiplayer <$> liftRunMessage msg attrs
