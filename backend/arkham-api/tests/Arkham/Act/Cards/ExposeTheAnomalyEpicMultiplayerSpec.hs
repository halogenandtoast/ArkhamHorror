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
    @chooseAmount@ 1..min(3, spendable clues), then on resolution it SPENDS those
    clues from the investigator (@spendClues@) straight into the shared pool and
    raises the shared @act-progress:1@ counter by the amount spent. Nothing is
    placed on the act -- it holds ZERO clue tokens. (In a non-event game the
    @RaiseShared@ delta is not mirrored back into scenario state, so we assert
    the emitted message rather than a count change.)
  * FIRST-RESOLVER (ability 2, @Objective $ forced $ RoundBegins #when@): once the
    mirrored pool reaches @2 * total@, this group advances its act in-group via
    the normal AdvanceAct side-A -> side-B flow, bumps the LOCAL @EpicActAdvances 1@,
    and raises @AdvanceRequested 1@ to signal the server. The POST-COMMIT server
    coordinator (not exercised here) consumes that signal, resets the pool, and
    bumps the generation -- so we deliberately do NOT assert any pool reset or
    generation bump.
  * FOLLOWER (ability 3, @Objective $ forced $ RoundBegins #when@): when the
    mirrored @act-advance-gen:1@ is ahead of this group's local @EpicActAdvances 1@,
    this group catches up by advancing in-group, bumping the local count but
    raising NO @AdvanceRequested@.

There are no local act clue tokens at any point (clues live in the shared pool),
so advancing does not clear any act clues.

Harness notes: with no configured act stack the side-B @advanceActDeck@ is a
no-op, and the Vulnerable Heart @leadChooseOneM@ has no Oozified locations to
target so it is skipped. We assert the in-group flip (side A -> B), the local
advance count, and the presence/absence of the server signal rather than the
act-deck position.
-}
realAct :: CardDef -> TestAppT Act
realAct def = do
  card <- genCard def
  let actId' = ActId (toCardCode card)
      act' = either (error . show) id $ lookupAct actId' 1 (toCardId card)
  overTest $ entitiesL . Entities.actsL %~ insertEntity act'
  pure act'

-- | Surface the contribution fast ability (index 1) under a during-turn window
-- and spend @amount@ of the investigator's clues into the shared pool.
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
  it "spends contributed clues from the investigator into the pool, leaving no clues on the act"
    . scenarioTest "85001"
    $ \self -> do
      act <- realAct Acts.exposeTheAnomalyEpicMultiplayer
      run $ PlaceTokens (TestSource mempty) (toTarget self) Clue 2

      raised <- createMessageChecker \case
        RaiseShared (SharedActProgress 1) n -> n == 2
        _ -> False

      contribute self act 2

      -- the contribution SPENDS the investigator's clues straight into the shared
      -- pool: the investigator empties and @act-progress:1@ is raised by the amount.
      -- Nothing is placed on the act (it holds zero clue tokens) and it does NOT
      -- advance.
      raised `refShouldBe` True
      self.clues `shouldReturn` 0
      field ActClues act.id `shouldReturn` 0
      assertAny $ ActWithSide A

  it "advances in-group and signals the server once the shared pool meets the global threshold (ability 2, first-resolver)"
    . scenarioTest "85001"
    $ \self -> do
      act <- realAct Acts.exposeTheAnomalyEpicMultiplayer
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
      -- ... bumps the LOCAL advance count and raises the server signal. The pool
      -- reset / generation bump are server-owned and intentionally NOT asserted here.
      scenarioCount (EpicActAdvances 1) `shouldReturn` 1
      requested `refShouldBe` True

  it "advances in-group with no server signal when the generation is ahead (ability 3, follower)"
    . scenarioTest "85001"
    $ \self -> do
      act <- realAct Acts.exposeTheAnomalyEpicMultiplayer
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

      -- the act flips in-group ...
      assertAny $ ActWithSide B
      assertNone $ ActWithSide A
      -- ... the local count catches up to the generation ...
      scenarioCount (EpicActAdvances 1) `shouldReturn` 1
      -- ... and the follower raises NO server signal.
      noSignal `refShouldBe` False
