module Arkham.Act.Cards.BlackwatersBaneEpicMultiplayer (blackwatersBaneEpicMultiplayer) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Epic.Types (
  SharedKey (AdvanceRequested, ActAdvanceGen, BlobStorySeed, SharedActProgress),
  sharedKeyText,
  totalInvestigatorsKey,
 )
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Investigator (getSpendableClueCount)
import Arkham.Helpers.Log (scenarioCount, scenarioCountIncrement)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Helpers.Story
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Placement
import Arkham.ScenarioLogKey (ScenarioCountKey (EpicActAdvances, EpicShared))
import Arkham.Story.Cards qualified as Stories
import Arkham.Token (Token (Clue))
import Arkham.Trait (Trait (Ooze, Oozified))

-- Epic Multiplayer variant of Blackwater's Bane (card 85008). Like the Epic
-- variant of Act 1, the clue requirement is a single GLOBAL pool shared across
-- every group: 2 clues per investigator across ALL groups (the event's frozen
-- total). Unlike the single-group act (which advances at the end of a round via
-- "A Moment of Respite"), the Epic variant advances by pooled clues: each
-- investigator has a fast ability to place up to 3 of their own clues onto this
-- act, and only clues placed on a group's act feed the shared @act-progress:3@
-- counter.
--
-- ADVANCE is FULLY IN-GROUP -- there is NO cross-group message injection. Each
-- group loops its own act deck (back to act 1) in its own normal AdvanceAct flow
-- at its own round begin:
--   * FIRST-RESOLVER (ability 2): once the pool reaches @2 * total@, this group
--     advances in-group and raises @AdvanceRequested 3@. The POST-COMMIT server
--     coordinator (under the event lock) consumes that signal, resets the pool to
--     0, and bumps @act-generation:3@ EXACTLY ONCE. We must NOT reset the pool or
--     bump the generation ourselves.
--   * FOLLOWER (ability 3): every other group advances on its OWN round begin once
--     the mirrored @act-generation:3@ is ahead of its local @EpicActAdvances 3@.
-- @EpicActAdvances 3@ is bumped on advance so the seeded story pick's @wave@
-- keeps re-rolling.
newtype BlackwatersBaneEpicMultiplayer = BlackwatersBaneEpicMultiplayer ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackwatersBaneEpicMultiplayer :: ActCard BlackwatersBaneEpicMultiplayer
blackwatersBaneEpicMultiplayer = act (3, A) BlackwatersBaneEpicMultiplayer Cards.blackwatersBaneEpicMultiplayer Nothing

-- This act's stage; the shared progress key and the local advance-count key derive
-- from it so they can't drift.
actStage :: Int
actStage = 3

-- The shared act-progress 'SharedKey', mirrored as @EpicShared "act-progress:3"@
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
-- @EpicShared "act-generation:3"@) is ahead of this group's local advance count, so
-- a resolver already crossed and this group still owes a flip (@generation - local >= 1@).
followerPendingCriterion :: Criterion
followerPendingCriterion =
  HasCalculation
    ( SubtractCalculation
        (ScenarioCount (EpicShared (sharedKeyText (ActAdvanceGen actStage))))
        (ScenarioCount actAdvancesKey)
    )
    (atLeast 1)

instance HasModifiersFor BlackwatersBaneEpicMultiplayer where
  getModifiersFor (BlackwatersBaneEpicMultiplayer a) =
    modifySelect a (EnemyWithTrait Ooze) [AddKeyword Keyword.Retaliate, ScenarioModifier "noBlob"]

instance HasAbilities BlackwatersBaneEpicMultiplayer where
  getAbilities (BlackwatersBaneEpicMultiplayer a) =
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

instance RunMessage BlackwatersBaneEpicMultiplayer where
  runMessage msg a@(BlackwatersBaneEpicMultiplayer attrs) = runQueueT $ case msg of
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
      pure $ BlackwatersBaneEpicMultiplayer $ attrs & metaL .~ toJSON True
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      -- FOLLOWER: the global generation is ahead of this group's local count, so a
      -- resolver already crossed. Catch up by advancing in-group. No shared writes.
      removeTokens (toSource attrs) attrs Clue attrs.clues
      scenarioCountIncrement actAdvancesKey
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- Only on the first advance: shuffle the set-aside Mi-Go Drones into the
      -- encounter deck along with the encounter discard pile.
      drones <- getSetAsideCardsMatching (cardIs Enemies.miGoDrone)
      unless (null drones) do
        shuffleSetAsideIntoEncounterDeck $ cardIs Enemies.miGoDrone
        shuffleEncounterDiscardBackIn
      n <- perPlayer 1
      selectEach (RevealedLocation <> LocationWithTrait Oozified <> LocationNotAtClueLimit) \loc ->
        push $ PlaceCluesUpToClueValue loc (toSource attrs) n
      -- Shared Part-1 story: every group derives the SAME pick from a single
      -- per-event seed (EpicShared "blob-story-seed", synced to all groups) plus this
      -- group's wave. EpicActAdvances was bumped by ability 2/3 before this flip, so
      -- it already reads the post-increment Kth value. All groups start at wave 1 and
      -- the seed is identical, so the first 3b in every group reads the same card;
      -- each loop (wave 2, 3, ...) re-rolls. `mod 4` is always 0..3.
      wave <- scenarioCount actAdvancesKey
      seed <- scenarioCount (EpicShared (sharedKeyText BlobStorySeed))
      lead <- getLead
      let chosen = case (seed + wave) `mod` 4 of
            0 -> Stories.rescueTheChemist
            1 -> Stories.recoverTheSample
            2 -> Stories.driveOffTheMiGo
            _ -> Stories.defuseTheExplosives
      readStoryWithPlacement_ lead chosen Global
      push $ ResetActDeckToStage 1
      pure a
    _ -> BlackwatersBaneEpicMultiplayer <$> liftRunMessage msg attrs
