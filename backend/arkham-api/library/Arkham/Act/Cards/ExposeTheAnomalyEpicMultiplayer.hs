module Arkham.Act.Cards.ExposeTheAnomalyEpicMultiplayer (exposeTheAnomalyEpicMultiplayer) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Epic.Types (
  SharedKey (AdvanceRequested, ActAdvanceGen, SharedActProgress),
  sharedKeyText,
  totalInvestigatorsKey,
 )
import Arkham.Helpers.Investigator (getSpendableClueCount)
import Arkham.Helpers.Log (scenarioCountIncrement)
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.ScenarioLogKey (ScenarioCountKey (EpicActAdvances, EpicShared))
import Arkham.Token (Token (Clue))
import Arkham.Trait (Trait (Oozified))

-- Epic Multiplayer variant of Expose the Anomaly (card 85005). The act's clue
-- requirement is a single GLOBAL pool shared across every group in the event:
-- 2 clues per investigator across ALL groups (the event's frozen total).
--
-- CONTRIBUTION: each investigator has a fast ability to place up to 3 of their
-- own clues onto this act. Only clues placed on a group's act feed the pool: each
-- placement raises @act-progress:1@ by the amount placed, so the pool is the live
-- sum of clues on every group's act.
--
-- ADVANCE is FULLY IN-GROUP -- there is NO cross-group message injection. Each
-- group flips its own act in its own normal AdvanceAct flow at its own round begin:
--   * FIRST-RESOLVER (ability 2): once the pool reaches @2 * total@, this group
--     advances in-group and raises @AdvanceRequested 1@. The POST-COMMIT server
--     coordinator (under the event lock) consumes that signal, resets the pool to
--     0, and bumps @act-generation:1@ EXACTLY ONCE. We must NOT reset the pool or
--     bump the generation ourselves.
--   * FOLLOWER (ability 3): every other group advances on its OWN round begin once
--     the mirrored @act-generation:1@ is ahead of its local @EpicActAdvances 1@.
-- The only cross-group communication is the shared counters (mirrored in), never a
-- game-message injected into another group.
newtype ExposeTheAnomalyEpicMultiplayer = ExposeTheAnomalyEpicMultiplayer ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exposeTheAnomalyEpicMultiplayer :: ActCard ExposeTheAnomalyEpicMultiplayer
exposeTheAnomalyEpicMultiplayer = act (1, A) ExposeTheAnomalyEpicMultiplayer Cards.exposeTheAnomalyEpicMultiplayer Nothing

-- This act's stage; the shared progress key and the local advance-count key derive
-- from it so they can't drift.
actStage :: Int
actStage = 1

-- The shared act-progress 'SharedKey', mirrored as @EpicShared "act-progress:1"@
-- and raised on each clue placement.
actProgressKey :: SharedKey
actProgressKey = SharedActProgress actStage

-- The LOCAL (per-group, never-synced) count of how many times this act has advanced.
actAdvancesKey :: ScenarioCountKey
actAdvancesKey = EpicActAdvances actStage

-- FIRST-RESOLVER availability: the shared pool has reached the global threshold of
-- 2 per investigator across the whole event (@pool - 2 * total >= 0@).
advanceReadyCriterion :: Criterion
advanceReadyCriterion =
  HasCalculation
    ( SubtractCalculation
        (ScenarioCount (EpicShared (sharedKeyText actProgressKey)))
        (MultiplyCalculation (Fixed 2) (ScenarioCount (EpicShared totalInvestigatorsKey)))
    )
    (atLeast 0)

-- FOLLOWER availability: the global advance generation (server-bumped, mirrored as
-- @EpicShared "act-generation:1"@) is ahead of this group's local advance count, so
-- a resolver already crossed and this group still owes a flip (@generation - local >= 1@).
followerPendingCriterion :: Criterion
followerPendingCriterion =
  HasCalculation
    ( SubtractCalculation
        (ScenarioCount (EpicShared (sharedKeyText (ActAdvanceGen actStage))))
        (ScenarioCount actAdvancesKey)
    )
    (atLeast 1)

instance HasAbilities ExposeTheAnomalyEpicMultiplayer where
  getAbilities (ExposeTheAnomalyEpicMultiplayer a) =
    -- CONTRIBUTION: fast, per investigator. Place up to 3 of your clues on the act.
    [restricted a 1 (DuringTurn You <> youExist InvestigatorWithAnyClues) $ FastAbility Free]
      <> [restricted a 2 (wrapCriteria a advanceReadyCriterion) $ Objective $ forced $ RoundBegins #when | onSide A a]
      <> [restricted a 3 followerPendingCriterion $ Objective $ forced $ RoundBegins #when | onSide A a]
   where
    -- Gate the first-resolver objective to fire ONCE per advance cycle: ability 2
    -- latches a per-act-instance meta flag, and while it is set this criterion
    -- collapses to Never (a shared/mirrored counter can't gate it -- the pool reset
    -- lags intra-action). The flag resets for free: the act is replaced on flip.
    wrapCriteria x = if toResultDefault False x.meta then const Never else id

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
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      -- FIRST-RESOLVER: the pool reached the global threshold. Consume this group's
      -- act clues, advance in-group via the normal flow, and signal the server. The
      -- server consumes AdvanceRequested under the lock, resets the pool to 0, and
      -- bumps the generation once -- we do NOT reset the pool or bump the generation.
      removeTokens (toSource attrs) attrs Clue attrs.clues
      scenarioCountIncrement actAdvancesKey
      push $ RaiseShared (AdvanceRequested actStage) 1
      advancedWithOther attrs
      pure $ ExposeTheAnomalyEpicMultiplayer $ attrs & metaL .~ toJSON True
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      -- FOLLOWER: the global generation is ahead of this group's local count, so a
      -- resolver already crossed. Catch up by advancing in-group. No shared writes.
      removeTokens (toSource attrs) attrs Clue attrs.clues
      scenarioCountIncrement actAdvancesKey
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      locations <- select $ LocationWithTrait Oozified
      leadChooseOneM $ targets locations $ createEnemyAt_ Enemies.vulnerableHeart
      advanceActDeck attrs
      pure a
    _ -> ExposeTheAnomalyEpicMultiplayer <$> liftRunMessage msg attrs
