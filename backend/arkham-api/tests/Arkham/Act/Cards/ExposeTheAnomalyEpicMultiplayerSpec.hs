module Arkham.Act.Cards.ExposeTheAnomalyEpicMultiplayerSpec (spec) where

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

{- | Regression for the Epic Multiplayer variant of Expose the Anomaly (act 85005,
Act 1) in The Blob That Ate Everything.

The clue requirement is a single GLOBAL pool shared across every group in the
event: 2 clues per investigator across ALL groups (the event's frozen total).
The advance is FULLY IN-GROUP -- there is no cross-group message injection; the
only cross-group communication is the mirrored shared counters. A single game
can therefore exercise the whole act-side contract:

  * CONTRIBUTION (ability 1, fast, @DuringTurn You@, only while you have clues):
    @chooseAmount@ 1..min(3, spendable clues), then on resolution it moves those
    clues from the investigator physically onto the act and raises the shared
    @act-progress:1@ counter by the amount placed. (In a non-event game the
    @RaiseShared@ delta is not mirrored back into scenario state, so we assert
    the emitted message rather than a count change.)
  * FIRST-RESOLVER (ability 2, @Objective $ forced $ RoundBegins #when@): once the
    mirrored pool reaches @2 * total@, this group advances its act in-group via
    the normal AdvanceAct side-A -> side-B flow, clears its own act clues, bumps
    the LOCAL @EpicActAdvances 1@, and raises @AdvanceRequested 1@ to signal the
    server. The POST-COMMIT server coordinator (not exercised here) consumes that
    signal, resets the pool, and bumps the generation -- so we deliberately do NOT
    assert any pool reset or generation bump.
  * FOLLOWER (ability 3, @Objective $ forced $ RoundBegins #when@): when the
    mirrored @act-advance-gen:1@ is ahead of this group's local @EpicActAdvances 1@,
    this group catches up by advancing in-group. It clears its clues and bumps the
    local count but raises NO @AdvanceRequested@.

Harness notes: with no configured act stack the side-B @advanceActDeck@ is a
no-op, and the Vulnerable Heart @leadChooseOneM@ has no Oozified locations to
target so it is skipped. We assert the in-group flip (side A -> B), the local
advance count, the cleared act clues, and the presence/absence of the server
signal rather than the act-deck position.
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
          "expected Expose the Anomaly's contribution ability (1) to be available"

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
        $ "expected Expose the Anomaly's act objective ability ("
        <> show idx
        <> ") to be available"

spec :: Spec
spec = describe "Expose the Anomaly (Epic Multiplayer)" do
  it "places contributed clues onto the act, drops them from the investigator, and raises the pool"
    . scenarioTest "85001"
    $ \self -> do
      act <- realAct Acts.exposeTheAnomalyEpicMultiplayer
      run $ PlaceTokens (TestSource mempty) (toTarget self) Clue 2

      raised <- createMessageChecker \case
        RaiseShared (SharedActProgress 1) n -> n == 2
        _ -> False

      contribute self act 2

      -- the contribution feeds the shared pool, physically moves the clues onto
      -- the act, and empties the investigator -- and does NOT advance the act.
      raised `refShouldBe` True
      field ActClues act.id `shouldReturn` 2
      self.clues `shouldReturn` 0
      assertAny $ ActWithSide A

  it "advances in-group and signals the server once the shared pool meets the global threshold (ability 2, first-resolver)"
    . scenarioTest "85001"
    $ \self -> do
      act <- realAct Acts.exposeTheAnomalyEpicMultiplayer
      -- physical act clues so we can prove the advance clears this group's act
      run $ PlaceTokens (TestSource mempty) (toTarget act) Clue 5
      scenarioCount (EpicActAdvances 1) `shouldReturn` 0

      -- mirror the shared pool at the global threshold: 2 per investigator with a
      -- frozen event total of 1 (pool 2 >= 2 * 1).
      run $ ScenarioCountSet (EpicShared "total-investigators") 1
      run $ ScenarioCountSet (EpicShared "act-progress:1") 2

      requested <- createMessageChecker \case
        RaiseShared (AdvanceRequested 1) n -> n == 1
        _ -> False

      useObjective self act 2

      -- the act flips in-group (side A -> B) ...
      assertAny $ ActWithSide B
      assertNone $ ActWithSide A
      -- ... clears this group's act clues, bumps the LOCAL advance count, and
      -- raises the server signal. The pool reset / generation bump are server-owned
      -- and intentionally NOT asserted here.
      field ActClues act.id `shouldReturn` 0
      scenarioCount (EpicActAdvances 1) `shouldReturn` 1
      requested `refShouldBe` True

  it "advances in-group with no server signal when the generation is ahead (ability 3, follower)"
    . scenarioTest "85001"
    $ \self -> do
      act <- realAct Acts.exposeTheAnomalyEpicMultiplayer
      run $ PlaceTokens (TestSource mempty) (toTarget act) Clue 5
      scenarioCount (EpicActAdvances 1) `shouldReturn` 0

      -- keep the first-resolver criterion FALSE (pool 0 < 2 * 2) and put the global
      -- generation ahead of this group's local advance count so only the follower
      -- path applies.
      run $ ScenarioCountSet (EpicShared "total-investigators") 2
      run $ ScenarioCountSet (EpicShared "act-progress:1") 0
      run $ ScenarioCountSet (EpicShared "act-advance-gen:1") 1

      noSignal <- createMessageChecker \case
        RaiseShared (AdvanceRequested 1) _ -> True
        _ -> False

      useObjective self act 3

      -- the act flips in-group and clears its clues ...
      assertAny $ ActWithSide B
      assertNone $ ActWithSide A
      field ActClues act.id `shouldReturn` 0
      -- ... the local count catches up to the generation ...
      scenarioCount (EpicActAdvances 1) `shouldReturn` 1
      -- ... and the follower raises NO server signal.
      noSignal `refShouldBe` False
