module Arkham.Act.Cards.BlackwatersBaneEpicMultiplayerSpec (spec) where

import Arkham.Act (lookupAct)
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence (ActSide (..))
import Arkham.Act.Types (Act, Field (ActClues))
import Arkham.Ability.Types (abilityIndex, abilitySource)
import Arkham.Entities qualified as Entities
import Arkham.Epic.Types (SharedKey (..))
import Arkham.Helpers.Action (getActions)
import Arkham.Helpers.Log (scenarioCount)
import Arkham.Matcher
import Arkham.Projection (field)
import Arkham.ScenarioLogKey (ScenarioCountKey (EpicActAdvances, EpicShared))
import Arkham.Token
import Arkham.Window (defaultWindows, mkWhen)
import Arkham.Window qualified as Window
import TestImport.New

{- | Regression for the Epic Multiplayer variant of Blackwater's Bane (act 85008,
Act 3) in The Blob That Ate Everything.

Same global-pool, fully-in-group contract as the Epic variant of Act 1: the clue
requirement is a single GLOBAL pool shared across every group (2 clues per
investigator across ALL groups), and each group flips its own act in its own
normal AdvanceAct flow. Unlike Act 1, the side-B flip loops the deck back to act
stage 1 and reads a seed-derived Part-1 story.

  * CONTRIBUTION (ability 1, fast, @DuringTurn You@, only while you have clues):
    @chooseAmount@ 1..min(3, spendable clues), then moves those clues physically
    onto the act and raises the shared @act-progress:3@ counter by the amount
    placed. (In a non-event game the @RaiseShared@ delta is not mirrored back into
    scenario state, so we assert the emitted message rather than a count change.)
  * FIRST-RESOLVER (ability 2, @Objective $ forced $ RoundBegins #when@): once the
    mirrored pool reaches @2 * total@, this group advances in-group, clears its own
    act clues, bumps the LOCAL @EpicActAdvances 3@, and raises @AdvanceRequested 3@.
    The pool reset / generation bump are server-owned and NOT asserted here.
  * FOLLOWER (ability 3, @Objective $ forced $ RoundBegins #when@): when the
    mirrored @act-advance-gen:3@ is ahead of this group's local @EpicActAdvances 3@,
    this group catches up by advancing in-group, clearing its clues and bumping the
    local count, raising NO @AdvanceRequested@.

The seeded Part-1 story pick is deterministic: the side-B handler reads
@wave = EpicActAdvances 3@ (already bumped to its post-increment value by ability
2/3 before the flip) and the per-event @blob-story-seed@, choosing
@(seed + wave) \`mod\` 4@ -> 0 rescueTheChemist / 1 recoverTheSample (85022) /
2 driveOffTheMiGo / 3 defuseTheExplosives.

Harness notes: the heavier side effects are inert here -- no set-aside Mi-Go
Drones to reshuffle, no revealed Oozified locations to seed, and
@ResetActDeckToStage 1@ is a no-op with no configured act stack -- so we assert
the in-group flip, the local advance count, the cleared act clues, the
presence/absence of the server signal, and the deterministic story read.
-}
realAct :: CardDef -> TestAppT Act
realAct def = do
  card <- genCard def
  let actId' = ActId (toCardCode card)
      act' = either (error . show) id $ lookupAct actId' 1 (toCardId card)
  overTest $ entitiesL . Entities.actsL %~ insertEntity act'
  pure act'

-- | Surface the contribution fast ability (index 1) under a during-turn window
-- and place @amount@ of the investigator's clues onto the act.
contribute :: Investigator -> Act -> Int -> TestAppT ()
contribute self act amount = do
  let ws = defaultWindows (toId self)
  abilities <-
    filter (\ab -> abilitySource ab == toSource act && abilityIndex ab == 1)
      <$> getActions (toId self) ws
  case abilities of
    (ability : _) -> do
      run $ UseAbility (toId self) ability ws
      resolveAmount self "Clues" amount
    [] ->
      liftIO
        $ expectationFailure
          "expected Blackwater's Bane's contribution ability (1) to be available"

-- | Surface an act objective (a forced @RoundBegins #when@ ability) by index and
-- use it under a round-begin window.
useObjective :: Investigator -> Act -> Int -> TestAppT ()
useObjective self act idx = do
  let ws = [mkWhen Window.AtBeginningOfRound]
  abilities <-
    filter (\ab -> abilitySource ab == toSource act && abilityIndex ab == idx)
      <$> getActions (toId self) ws
  case abilities of
    (ability : _) -> run $ UseAbility (toId self) ability ws
    [] ->
      liftIO
        $ expectationFailure
        $ "expected Blackwater's Bane's act objective ability ("
        <> show idx
        <> ") to be available"

spec :: Spec
spec = describe "Blackwater's Bane (Epic Multiplayer)" do
  it "places contributed clues onto the act, drops them from the investigator, and raises the pool"
    . scenarioTest "85001"
    $ \self -> do
      act <- realAct Acts.blackwatersBaneEpicMultiplayer
      run $ PlaceTokens (TestSource mempty) (toTarget self) Clue 2

      raised <- createMessageChecker \case
        RaiseShared (SharedActProgress 3) n -> n == 2
        _ -> False

      contribute self act 2

      raised `refShouldBe` True
      field ActClues act.id `shouldReturn` 2
      self.clues `shouldReturn` 0
      assertAny $ ActWithSide A

  it "advances in-group and signals the server once the shared pool meets the global threshold (ability 2, first-resolver)"
    . scenarioTest "85001"
    $ \self -> do
      act <- realAct Acts.blackwatersBaneEpicMultiplayer
      run $ PlaceTokens (TestSource mempty) (toTarget act) Clue 5
      scenarioCount (EpicActAdvances 3) `shouldReturn` 0

      -- mirror the shared pool at the global threshold (pool 2 >= 2 * 1).
      run $ ScenarioCountSet (EpicShared "total-investigators") 1
      run $ ScenarioCountSet (EpicShared "act-progress:3") 2

      requested <- createMessageChecker \case
        RaiseShared (AdvanceRequested 3) n -> n == 1
        _ -> False

      useObjective self act 2

      assertAny $ ActWithSide B
      assertNone $ ActWithSide A
      field ActClues act.id `shouldReturn` 0
      scenarioCount (EpicActAdvances 3) `shouldReturn` 1
      requested `refShouldBe` True

  it "advances in-group with no server signal when the generation is ahead (ability 3, follower)"
    . scenarioTest "85001"
    $ \self -> do
      act <- realAct Acts.blackwatersBaneEpicMultiplayer
      run $ PlaceTokens (TestSource mempty) (toTarget act) Clue 5
      scenarioCount (EpicActAdvances 3) `shouldReturn` 0

      -- keep the first-resolver criterion FALSE (pool 0 < 2 * 2) and put the global
      -- generation ahead of this group's local advance count so only the follower
      -- path applies.
      run $ ScenarioCountSet (EpicShared "total-investigators") 2
      run $ ScenarioCountSet (EpicShared "act-progress:3") 0
      run $ ScenarioCountSet (EpicShared "act-advance-gen:3") 1

      noSignal <- createMessageChecker \case
        RaiseShared (AdvanceRequested 3) _ -> True
        _ -> False

      useObjective self act 3

      assertAny $ ActWithSide B
      assertNone $ ActWithSide A
      field ActClues act.id `shouldReturn` 0
      scenarioCount (EpicActAdvances 3) `shouldReturn` 1
      noSignal `refShouldBe` False

  it "reads the seed-derived Part-1 story on the in-group flip (seed 0, wave 1 -> Recover the Sample 85022)"
    . scenarioTest "85001"
    $ \self -> do
      act <- realAct Acts.blackwatersBaneEpicMultiplayer
      run $ ScenarioCountSet (EpicShared "blob-story-seed") 0
      run $ ScenarioCountSet (EpicShared "total-investigators") 1
      run $ ScenarioCountSet (EpicShared "act-progress:3") 2
      run $ PlaceTokens (TestSource mempty) (toTarget act) Clue 4

      -- ability 2 bumps EpicActAdvances 3 (0 -> 1) BEFORE the flip, so the side-B
      -- handler reads wave = 1; (seed 0 + wave 1) `mod` 4 = 1 -> 85022. The story
      -- read lives in the side-B AdvanceAct; createMessageChecker fires on dequeue,
      -- so the assertion holds regardless of how far the story resolves here.
      readsStory <- createMessageChecker \case
        StoryMessage (ReadStoryWithPlacement _ c _ _ _) -> toCardCode c == "85022"
        _ -> False

      useObjective self act 2
      -- resolve the normal AdvanceAct side-A -> side-B choice so the side-B flip
      -- (and its story read) runs.
      chooseOnlyOption "advance the act"

      readsStory `refShouldBe` True
