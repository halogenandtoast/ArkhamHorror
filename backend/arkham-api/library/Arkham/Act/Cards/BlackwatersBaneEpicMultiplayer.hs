module Arkham.Act.Cards.BlackwatersBaneEpicMultiplayer (blackwatersBaneEpicMultiplayer) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Epic.Types (
  GroupOrdinal (GroupOrdinal),
  SharedKey (ActAdvanceGen, ActContribution, ActSpend, AdvanceRequested, BlobStorySeed, SharedActProgress),
  groupOrdinalKey,
  sharedKeyText,
  totalInvestigatorsKey,
 )
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Investigator (getSpendableClueCount)
import Arkham.Helpers.Log (scenarioCount, scenarioCountIncrement)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (getInvestigators, getSetAsideCardsMatching)
import Arkham.Helpers.Story
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.ScenarioLogKey (ScenarioCountKey (EpicActAdvances, EpicShared))
import Arkham.Story.Cards qualified as Stories
import Arkham.Trait (Trait (Ooze, Oozified))

-- Epic Multiplayer variant of Blackwater's Bane (card 85008). Like the Epic variant
-- of Act 1, the clue requirement is a single GLOBAL pool shared across every group:
-- 2 clues per investigator across ALL groups (the event's frozen total).
--
-- CONTRIBUTION (ability 1): each investigator spends up to 3 of their own clues into
-- the shared pool. The clues are spent FROM THE INVESTIGATOR (no local act tokens);
-- each spend raises both the global @act-progress:3@ pool AND this group's own
-- @act-contribution:3:<ordinal>@ so the organizer can cap each group's spend.
--
-- ADVANCE is FULLY IN-GROUP -- no cross-group message injection:
--   * FIRST-RESOLVER (ability 2): once the pool reaches @2 * total@, raise
--     @AdvanceRequested 3@ and PARK on a Continue choice. The server sets
--     AwaitingOrganizer; once the organizer allocates each group's @act-spend:3:o@
--     and releases, the lead answers Continue, which settles and advances in-group.
--   * FOLLOWER (ability 3): every other group advances on its OWN round begin once
--     the server-bumped @act-generation:3@ is ahead of its local @EpicActAdvances 3@.
-- On advance, each group returns its leftover clues (contributed - spent) to its OWN
-- investigators (in-group), bumps @EpicActAdvances 3@ (so the seeded story @wave@
-- re-rolls), then loops the deck back to act 1 via the normal AdvanceAct flow.
newtype BlackwatersBaneEpicMultiplayer = BlackwatersBaneEpicMultiplayer ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackwatersBaneEpicMultiplayer :: ActCard BlackwatersBaneEpicMultiplayer
blackwatersBaneEpicMultiplayer = act (3, A) BlackwatersBaneEpicMultiplayer Cards.blackwatersBaneEpicMultiplayer Nothing

-- This act's stage; the shared keys and the local advance-count key derive from it.
actStage :: Int
actStage = 3

-- The shared act-progress 'SharedKey' (the global pool), raised on each clue spend.
actProgressKey :: SharedKey
actProgressKey = SharedActProgress actStage

-- The LOCAL (per-group, never-synced) count of how many times this act has advanced.
actAdvancesKey :: ScenarioCountKey
actAdvancesKey = EpicActAdvances actStage

-- FIRST-RESOLVER availability: the shared pool reached the global threshold of 2 per
-- investigator across the whole event (@pool - 2 * total >= 0@).
advanceReadyCriterion :: Criterion
advanceReadyCriterion =
  HasCalculation
    ( SubtractCalculation
        (ScenarioCount (EpicShared (sharedKeyText actProgressKey)))
        (MultiplyCalculation (Fixed 2) (ScenarioCount (EpicShared totalInvestigatorsKey)))
    )
    (atLeast 0)

-- FOLLOWER availability: the server-bumped global generation (mirrored as
-- @EpicShared "act-generation:3"@) is ahead of this group's local advance count.
followerPendingCriterion :: Criterion
followerPendingCriterion =
  HasCalculation
    ( SubtractCalculation
        (ScenarioCount (EpicShared (sharedKeyText (ActAdvanceGen actStage))))
        (ScenarioCount actAdvancesKey)
    )
    (atLeast 1)

-- Settle this group's advance, then advance in-group. Returns the group's leftover
-- clues (its own contribution minus the organizer-allocated spend) to its OWN
-- investigators, bumps the local advance count, then advances via the normal flow.
-- Reads ActSpend (the organizer endpoint mirrors it before release), so it runs from
-- the Continue option / ability 3 -- NOT the AdvanceAct side-B body. NO shared writes.
returnLeftoverAndAdvance :: ReverseQueue m => ActAttrs -> m ()
returnLeftoverAndAdvance attrs = do
  ordinal <- scenarioCount (EpicShared groupOrdinalKey)
  spent <- scenarioCount (EpicShared (sharedKeyText (ActSpend actStage (GroupOrdinal ordinal))))
  contributed <- scenarioCount (EpicShared (sharedKeyText (ActContribution actStage (GroupOrdinal ordinal))))
  let leftover = contributed - spent
  scenarioCountIncrement actAdvancesKey
  when (leftover > 0) do
    investigators <- getInvestigators
    case investigators of
      [single] -> gainClues single (toSource attrs) leftover
      _ -> leadChooseOneM $ targets investigators \i -> gainClues i (toSource attrs) leftover
  advancedWithOther attrs

instance HasModifiersFor BlackwatersBaneEpicMultiplayer where
  getModifiersFor (BlackwatersBaneEpicMultiplayer a) =
    modifySelect a (EnemyWithTrait Ooze) [AddKeyword Keyword.Retaliate, ScenarioModifier "noBlob"]

instance HasAbilities BlackwatersBaneEpicMultiplayer where
  getAbilities (BlackwatersBaneEpicMultiplayer a) =
    -- CONTRIBUTION: fast, per investigator. Spend up to 3 of your clues to the pool.
    [restricted a 1 (DuringTurn You <> youExist InvestigatorWithAnyClues) $ FastAbility Free]
      <> [restricted a 2 (wrapCriteria a advanceReadyCriterion) $ Objective $ forced $ RoundBegins #when | onSide A a]
      <> [restricted a 3 followerPendingCriterion $ Objective $ forced $ RoundBegins #when | onSide A a]
   where
    -- Gate the first-resolver objective to fire ONCE per advance cycle: ability 2
    -- latches a per-act-instance meta flag, collapsing this criterion to Never until
    -- the act is replaced on flip (so it can't re-park before the server settles).
    wrapCriteria x = if toResultDefault False x.meta then const Never else id

instance RunMessage BlackwatersBaneEpicMultiplayer where
  runMessage msg a@(BlackwatersBaneEpicMultiplayer attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- min 3 <$> getSpendableClueCount iid
      when (n > 0) $ chooseAmount iid "Clues" "Clues" 1 n attrs
      pure a
    ResolveAmounts iid (getChoiceAmount "Clues" -> amount) (isTarget attrs -> True) | amount > 0 -> do
      -- Spend the chosen clues into the shared pool AND record this group's own
      -- contribution (so the organizer can cap each group's spend). No local tokens.
      ordinal <- scenarioCount (EpicShared groupOrdinalKey)
      spendClues iid amount
      push $ RaiseShared actProgressKey amount
      push $ RaiseShared (ActContribution actStage (GroupOrdinal ordinal)) amount
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      -- FIRST-RESOLVER: request the organizer allocation and PARK. Do NOT advance or
      -- increment here. leadChooseOneM parks the single option via the raw parking
      -- chooseOne (NOT chooseOrRun*); the Continue defers via NextAdvanceActStep so the
      -- settle helper reads the freshly-mirrored ActSpend at answer time.
      push $ RaiseShared (AdvanceRequested actStage) 1
      leadChooseOneM $ labeled "$continue" $ push $ NextAdvanceActStep attrs.id 1
      pure $ BlackwatersBaneEpicMultiplayer $ attrs & metaL .~ toJSON True
    NextAdvanceActStep aid 1 | aid == attrs.id -> do
      -- The parked Continue, answered after the organizer allocated ActSpend.
      returnLeftoverAndAdvance attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      -- FOLLOWER: the global generation is ahead of this group's local count. Settle
      -- and advance in-group via the same helper. (A parked resolver is in its Continue
      -- choice; once it settles, its local count catches up so this no longer applies.)
      returnLeftoverAndAdvance attrs
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
      -- Shared Part-1 story: every group derives the SAME pick from a single per-event
      -- seed (EpicShared "blob-story-seed") plus this group's wave. EpicActAdvances was
      -- bumped by the settle helper before this flip, so it reads the post-increment
      -- Kth value. All groups start at wave 1 with the same seed, so the first 3b in
      -- every group reads the same card; each loop re-rolls. `mod 4` is always 0..3.
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
