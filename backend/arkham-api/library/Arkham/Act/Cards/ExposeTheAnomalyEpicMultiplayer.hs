module Arkham.Act.Cards.ExposeTheAnomalyEpicMultiplayer (exposeTheAnomalyEpicMultiplayer) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Epic.Types (SharedKey (SharedActProgress))
import Arkham.Helpers.Investigator (getSpendableClueCount)
import Arkham.Helpers.Log (scenarioCountIncrement)
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.ScenarioLogKey (ScenarioCountKey (EpicActAdvances))
import Arkham.Token (Token (Clue))
import Arkham.Trait (Trait (Oozified))

-- Epic Multiplayer variant of Expose the Anomaly (card 85005). The act's clue
-- requirement is a single GLOBAL pool shared across every group in the event:
-- 2 clues per investigator across ALL groups (the event's frozen total).
--
-- CONTRIBUTION: each investigator has a fast ability to place up to 3 of their
-- own clues onto this act. ONLY clues actually placed on a group's act feed the
-- pool: each placement raises the shared @act-progress:1@ counter by the amount
-- placed, so the counter is the live SUM of clues currently on every group's act.
--
-- ADVANCE is SEAM-COORDINATED. The cross-group seam detects @pool >= 2 * total@,
-- resets the pool, and delivers @ResolveEpicActAdvance 1 spendAmount@ to this act
-- in every group. We consume @spendAmount@ from the act, hand the leftover clues
-- to this group's investigators, and advance the deck directly (no AdvanceAct
-- confirmation, which would park in non-interactively synced groups). We never
-- touch a Shared* counter here -- the seam owns the pool reset.
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
-- @EpicShared "act-progress:1"@ and raised on each clue placement.
actProgressKey :: SharedKey
actProgressKey = SharedActProgress actStage

-- The LOCAL (per-group, never-synced) count of how many times this act has advanced.
actAdvancesKey :: ScenarioCountKey
actAdvancesKey = EpicActAdvances actStage

instance HasAbilities ExposeTheAnomalyEpicMultiplayer where
  getAbilities (ExposeTheAnomalyEpicMultiplayer a) =
    -- CONTRIBUTION: fast, per investigator. Place up to 3 of your clues on the act.
    [restricted a 1 (DuringTurn You <> youExist InvestigatorWithAnyClues) $ FastAbility Free]

instance RunMessage ExposeTheAnomalyEpicMultiplayer where
  runMessage msg a@(ExposeTheAnomalyEpicMultiplayer attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- min 3 <$> getSpendableClueCount iid
      when (n > 0) $ chooseAmount iid "Clues" "Clues" 1 n attrs
      pure a
    ResolveAmounts iid (getChoiceAmount "Clues" -> amount) (isTarget attrs -> True) | amount > 0 -> do
      -- Place this investigator's chosen clues onto the act, then add them to the
      -- shared pool. Only clues placed on the act count.
      moveTokens (attrs.ability 1) iid attrs Clue amount
      push $ RaiseShared actProgressKey amount
      pure a
    ResolveEpicActAdvance stage spendAmount | stage == actStage -> do
      -- The seam has consumed `spendAmount` from the global pool; the leftover clues
      -- sitting on THIS group's act go to this group's investigators (they take
      -- control). The act's own clue tokens are discarded by the deck swap below;
      -- we re-grant only the leftover, so `spendAmount` is effectively spent.
      let leftover = max 0 (attrs.clues - spendAmount)
      when (leftover > 0) do
        iids <- select UneliminatedInvestigator
        unless (null iids) do
          let numInvestigators = length iids
              base = leftover `div` numInvestigators
              extra = leftover `mod` numInvestigators
          for_ (zip [0 ..] iids) \(i, iid) -> do
            let amt = base + if i < extra then 1 else 0
            when (amt > 0) $ gainClues iid (toSource attrs) amt
      -- Keep the per-act advance count (stage 3 uses it for its story wave; kept
      -- here for symmetry). Never thresholded here -- the seam drives the advance.
      scenarioCountIncrement actAdvancesKey
      -- Advance the deck directly (no AdvanceAct confirmation), then the side-effect
      -- choice. The deck progresses before the parking choice so idle synced groups
      -- still fully advance.
      advanceActDeck attrs
      locations <- select $ LocationWithTrait Oozified
      leadChooseOneM $ targets locations $ createEnemyAt_ Enemies.vulnerableHeart
      pure a
    _ -> ExposeTheAnomalyEpicMultiplayer <$> liftRunMessage msg attrs
