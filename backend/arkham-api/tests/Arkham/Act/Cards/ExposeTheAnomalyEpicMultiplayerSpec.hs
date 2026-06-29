module Arkham.Act.Cards.ExposeTheAnomalyEpicMultiplayerSpec (spec) where

import Arkham.Act (lookupAct)
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence (ActSide (..))
import Arkham.Act.Types (Act, Field (ActClues))
import Arkham.Ability.Types (abilityIndex, abilitySource)
import Arkham.Classes.HasGame (getGame)
import Arkham.Entities qualified as Entities
import Arkham.Epic.Types (GroupOrdinal (..), SharedKey (..))
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
event (2 clues per investigator across ALL groups). Advancing is FULLY IN-GROUP
but gated on an ORGANIZER excess-allocation step, so the resolver PARKS until the
organizer allocates each group's spend. The act-side contract a single game can
exercise:

  * CONTRIBUTION (ability 1, fast, @DuringTurn You@, only while you have clues):
    @chooseAmount@ 1..min(3, spendable clues), then on resolution @spendClues@ the
    chosen clues from the investigator (no local act tokens) and raises BOTH the
    global pool @act-progress:1@ AND this group's own @act-contribution:1:<ordinal>@
    (ordinal read from @EpicShared "group-ordinal"@) so the organizer can cap each
    group's spend. (In a non-event game @RaiseShared@ is inert, so we assert the
    emitted messages rather than mirrored counts.)
  * FIRST-RESOLVER (ability 2, @Objective $ forced $ RoundBegins #when@): once the
    pool reaches @2 * total@, it raises @AdvanceRequested 1@, latches a meta flag,
    and PARKS on a single-option @$continue@ choice (@leadChooseOneM $ labeled
    "$continue"@). It does NOT advance and does NOT increment here -- a single-option
    chooseOne genuinely parks until answered.
  * SETTLE (the parked Continue pushes @NextAdvanceActStep act 1@; also ability 3's
    body): returns this group's leftover @contributed - spent@ clues to its OWN
    investigators (auto for a solo group, lead picks otherwise), increments the LOCAL
    @EpicActAdvances 1@, then advances via the normal AdvanceAct side-A -> side-B flow.
  * FOLLOWER (ability 3, @Objective $ forced $ RoundBegins #when@): when the
    server-bumped @act-advance-gen:1@ is ahead of the local @EpicActAdvances 1@, it
    runs the SAME settle helper, raising NO @AdvanceRequested@.

Seam-level (server) and NOT reachable from a single-game spec: the organizer
allocation endpoint, the @awaiting-organizer:1@ gate / release, the pool reset and
@act-advance-gen:1@ bump, and cross-group mirror ordering / concurrency. We
simulate the organizer's allocation by @ScenarioCountSet@ on the mirrored keys
(@act-contribution:1:o@, @act-spend:1:o@, @act-advance-gen:1@) and drive the parked
step directly with @NextAdvanceActStep@.

Harness notes: with no configured act stack the side-B @advanceActDeck@ is a
no-op, and the Vulnerable Heart @leadChooseOneM@ has no Oozified locations to
target so it is skipped. We assert the in-group flip (side A -> B), the local
advance count, the returned leftover clues, and the presence/absence of the
server signal rather than the act-deck position.
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

-- | The option labels of every currently-pending question (display wrappers
-- stripped). Used to prove the first-resolver genuinely PARKS on its @$continue@.
pendingLabels :: TestAppT [Text]
pendingLabels = do
  questions <- toList . gameQuestion <$> getGame
  pure $ concatMap (labelsOf . stripQuestionWrappers) questions
 where
  labelsOf = \case
    ChooseOne uis -> mapMaybe uiLabel uis
    PlayerWindowChooseOne uis -> mapMaybe uiLabel uis
    _ -> []
  uiLabel = \case
    Label l _ -> Just l
    _ -> Nothing

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

      -- the contribution SPENDS the investigator's clues into the shared pool: the
      -- investigator empties and the global @act-progress:1@ pool is raised. Nothing
      -- is placed on the act (zero clue tokens) and it does NOT advance.
      raised `refShouldBe` True
      self.clues `shouldReturn` 0
      field ActClues act.id `shouldReturn` 0
      assertAny $ ActWithSide A

  it "records the group's own contribution under its ordinal so the organizer can cap the spend"
    . scenarioTest "85001"
    $ \self -> do
      act <- realAct Acts.exposeTheAnomalyEpicMultiplayer
      run $ PlaceTokens (TestSource mempty) (toTarget self) Clue 2
      -- this group's mirrored ordinal; the contribution key is suffixed with it.
      run $ ScenarioCountSet (EpicShared "group-ordinal") 2

      recorded <- createMessageChecker \case
        RaiseShared (ActContribution 1 (GroupOrdinal 2)) n -> n == 2
        _ -> False

      contribute self act 2

      recorded `refShouldBe` True
      self.clues `shouldReturn` 0

  it "PARKS the first resolver on a Continue choice without advancing (ability 2)"
    . scenarioTest "85001"
    $ \self -> do
      act <- realAct Acts.exposeTheAnomalyEpicMultiplayer
      scenarioCount (EpicActAdvances 1) `shouldReturn` 0

      -- pool at the global threshold (pool 2 >= 2 * 1).
      run $ ScenarioCountSet (EpicShared "total-investigators") 1
      run $ ScenarioCountSet (EpicShared "act-progress:1") 2

      requested <- createMessageChecker \case
        RaiseShared (AdvanceRequested 1) n -> n == 1
        _ -> False

      useObjective self act 2

      -- it requests the organizer allocation and PARKS: the single-option Continue
      -- choice is genuinely pending, the act stays on side A, and the local advance
      -- count is NOT incremented (the server settles it later via the parked step).
      requested `refShouldBe` True
      pendingLabels `shouldReturn` ["$continue"]
      assertAny $ ActWithSide A
      assertNone $ ActWithSide B
      scenarioCount (EpicActAdvances 1) `shouldReturn` 0

  it "settles the parked step: returns leftover clues, increments, and advances (contributed 3, spent 2 -> 1 back)"
    . scenarioTest "85001"
    $ \self -> do
      act <- realAct Acts.exposeTheAnomalyEpicMultiplayer
      self.clues `shouldReturn` 0
      scenarioCount (EpicActAdvances 1) `shouldReturn` 0

      -- simulate the organizer's allocation for this group (ordinal 0 by default):
      -- contributed 3, allocated spend 2.
      run $ ScenarioCountSet (EpicShared "act-contribution:1:0") 3
      run $ ScenarioCountSet (EpicShared "act-spend:1:0") 2

      -- the parked Continue defers to this step, read after the allocation mirrors.
      run $ NextAdvanceActStep act.id 1

      -- the leftover (3 - 2 = 1) returns to the solo group's investigator, the local
      -- count increments, and the act advances in-group (side A -> B).
      self.clues `shouldReturn` 1
      scenarioCount (EpicActAdvances 1) `shouldReturn` 1
      assertAny $ ActWithSide B
      assertNone $ ActWithSide A

  it "settles with no leftover when the whole contribution is allocated (contributed 2, spent 2 -> 0 back)"
    . scenarioTest "85001"
    $ \self -> do
      act <- realAct Acts.exposeTheAnomalyEpicMultiplayer
      run $ ScenarioCountSet (EpicShared "act-contribution:1:0") 2
      run $ ScenarioCountSet (EpicShared "act-spend:1:0") 2

      run $ NextAdvanceActStep act.id 1

      -- no clues are returned (leftover 0) but the act still increments and advances.
      self.clues `shouldReturn` 0
      scenarioCount (EpicActAdvances 1) `shouldReturn` 1
      assertAny $ ActWithSide B
      assertNone $ ActWithSide A

  it "advances a follower in-group with no server signal when the generation is ahead (ability 3)"
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

      -- the follower runs the same settle helper: it advances in-group and catches
      -- its local count up to the generation, but raises NO server signal.
      assertAny $ ActWithSide B
      assertNone $ ActWithSide A
      scenarioCount (EpicActAdvances 1) `shouldReturn` 1
      noSignal `refShouldBe` False
