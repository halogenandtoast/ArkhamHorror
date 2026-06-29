module Arkham.Act.Cards.BlackwatersBaneEpicMultiplayerSpec (spec) where

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

{- | Regression for the Epic Multiplayer variant of Blackwater's Bane (act 85008,
Act 3) in The Blob That Ate Everything.

Same global-pool, organizer-gated, fully-in-group contract as the Epic variant of
Act 1: a single GLOBAL pool (2 clues per investigator across ALL groups), the
first resolver PARKS until the organizer allocates each group's spend, and each
group advances its own act. Unlike Act 1, the side-B flip loops the deck back to
act stage 1 and reads a seed-derived Part-1 story.

  * CONTRIBUTION (ability 1, fast, @DuringTurn You@, only while you have clues):
    @chooseAmount@ 1..min(3, spendable clues), then @spendClues@ from the
    investigator (no local act tokens) and raises BOTH the global pool
    @act-progress:3@ AND this group's @act-contribution:3:<ordinal>@ (ordinal from
    @EpicShared "group-ordinal"@). (In a non-event game @RaiseShared@ is inert, so we
    assert the emitted messages.)
  * FIRST-RESOLVER (ability 2, @Objective $ forced $ RoundBegins #when@): once the
    pool reaches @2 * total@, it raises @AdvanceRequested 3@, latches a meta flag,
    and PARKS on a single-option @$continue@ choice without advancing or
    incrementing.
  * SETTLE (the parked Continue pushes @NextAdvanceActStep act 1@; also ability 3's
    body): returns this group's leftover @contributed - spent@ clues to its OWN
    investigators, increments the LOCAL @EpicActAdvances 3@ (so the seeded story
    @wave@ re-rolls), then loops the deck back via the normal AdvanceAct flow.
  * FOLLOWER (ability 3, @Objective $ forced $ RoundBegins #when@): when the
    server-bumped @act-advance-gen:3@ is ahead of the local @EpicActAdvances 3@, it
    runs the SAME settle helper, raising NO @AdvanceRequested@.

The seeded Part-1 story pick is deterministic: the side-B handler reads
@wave = EpicActAdvances 3@ (already bumped to its post-increment value by the settle
helper before the flip) and the per-event @blob-story-seed@, choosing
@(seed + wave) \`mod\` 4@ -> 0 rescueTheChemist / 1 recoverTheSample (85022) /
2 driveOffTheMiGo / 3 defuseTheExplosives.

Seam-level (server) and NOT reachable from a single-game spec: the organizer
allocation endpoint, the @awaiting-organizer:3@ gate / release, the pool reset and
@act-advance-gen:3@ bump, and cross-group mirror ordering / concurrency. We
simulate the organizer's allocation with @ScenarioCountSet@ on the mirrored keys
and drive the parked step directly with @NextAdvanceActStep@.

Harness notes: the heavier side effects are inert here -- no set-aside Mi-Go
Drones to reshuffle, no revealed Oozified locations to seed, and
@ResetActDeckToStage 1@ is a no-op with no configured act stack -- so we assert
the in-group flip, the local advance count, the returned leftover clues, the
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
spec = describe "Blackwater's Bane (Epic Multiplayer)" do
  it "spends contributed clues from the investigator into the pool, leaving no clues on the act"
    . scenarioTest "85001"
    $ \self -> do
      act <- realAct Acts.blackwatersBaneEpicMultiplayer
      run $ PlaceTokens (TestSource mempty) (toTarget self) Clue 2

      raised <- createMessageChecker \case
        RaiseShared (SharedActProgress 3) n -> n == 2
        _ -> False

      contribute self act 2

      -- the contribution SPENDS the investigator's clues into the shared pool: the
      -- investigator empties and the global @act-progress:3@ pool is raised. Nothing
      -- is placed on the act (zero clue tokens) and it does NOT advance.
      raised `refShouldBe` True
      self.clues `shouldReturn` 0
      field ActClues act.id `shouldReturn` 0
      assertAny $ ActWithSide A

  it "records the group's own contribution under its ordinal so the organizer can cap the spend"
    . scenarioTest "85001"
    $ \self -> do
      act <- realAct Acts.blackwatersBaneEpicMultiplayer
      run $ PlaceTokens (TestSource mempty) (toTarget self) Clue 2
      run $ ScenarioCountSet (EpicShared "group-ordinal") 2

      recorded <- createMessageChecker \case
        RaiseShared (ActContribution 3 (GroupOrdinal 2)) n -> n == 2
        _ -> False

      contribute self act 2

      recorded `refShouldBe` True
      self.clues `shouldReturn` 0

  it "PARKS the first resolver on a Continue choice without advancing (ability 2)"
    . scenarioTest "85001"
    $ \self -> do
      act <- realAct Acts.blackwatersBaneEpicMultiplayer
      scenarioCount (EpicActAdvances 3) `shouldReturn` 0

      run $ ScenarioCountSet (EpicShared "total-investigators") 1
      run $ ScenarioCountSet (EpicShared "act-progress:3") 2

      requested <- createMessageChecker \case
        RaiseShared (AdvanceRequested 3) n -> n == 1
        _ -> False

      useObjective self act 2

      requested `refShouldBe` True
      pendingLabels `shouldReturn` ["$continue"]
      assertAny $ ActWithSide A
      assertNone $ ActWithSide B
      scenarioCount (EpicActAdvances 3) `shouldReturn` 0

  it "settles the parked step: returns leftover clues, increments, and advances (contributed 3, spent 2 -> 1 back)"
    . scenarioTest "85001"
    $ \self -> do
      act <- realAct Acts.blackwatersBaneEpicMultiplayer
      self.clues `shouldReturn` 0
      scenarioCount (EpicActAdvances 3) `shouldReturn` 0

      run $ ScenarioCountSet (EpicShared "act-contribution:3:0") 3
      run $ ScenarioCountSet (EpicShared "act-spend:3:0") 2

      run $ NextAdvanceActStep act.id 1

      self.clues `shouldReturn` 1
      scenarioCount (EpicActAdvances 3) `shouldReturn` 1
      assertAny $ ActWithSide B
      assertNone $ ActWithSide A

  it "settles with no leftover when the whole contribution is allocated (contributed 2, spent 2 -> 0 back)"
    . scenarioTest "85001"
    $ \self -> do
      act <- realAct Acts.blackwatersBaneEpicMultiplayer
      run $ ScenarioCountSet (EpicShared "act-contribution:3:0") 2
      run $ ScenarioCountSet (EpicShared "act-spend:3:0") 2

      run $ NextAdvanceActStep act.id 1

      self.clues `shouldReturn` 0
      scenarioCount (EpicActAdvances 3) `shouldReturn` 1
      assertAny $ ActWithSide B
      assertNone $ ActWithSide A

  it "advances a follower in-group with no server signal when the generation is ahead (ability 3)"
    . scenarioTest "85001"
    $ \self -> do
      act <- realAct Acts.blackwatersBaneEpicMultiplayer
      scenarioCount (EpicActAdvances 3) `shouldReturn` 0

      run $ ScenarioCountSet (EpicShared "total-investigators") 2
      run $ ScenarioCountSet (EpicShared "act-progress:3") 0
      run $ ScenarioCountSet (EpicShared "act-advance-gen:3") 1

      noSignal <- createMessageChecker \case
        RaiseShared (AdvanceRequested 3) _ -> True
        _ -> False

      useObjective self act 3

      assertAny $ ActWithSide B
      assertNone $ ActWithSide A
      scenarioCount (EpicActAdvances 3) `shouldReturn` 1
      noSignal `refShouldBe` False

  it "reads the seed-derived Part-1 story on the in-group flip (seed 0, wave 1 -> Recover the Sample 85022)"
    . scenarioTest "85001"
    $ \_ -> do
      act <- realAct Acts.blackwatersBaneEpicMultiplayer
      run $ ScenarioCountSet (EpicShared "blob-story-seed") 0

      -- the settle helper bumps EpicActAdvances 3 (0 -> 1) BEFORE the flip, so the
      -- side-B handler reads wave = 1; (seed 0 + wave 1) `mod` 4 = 1 -> 85022.
      -- createMessageChecker fires on dequeue, so it holds regardless of how far the
      -- regenerated story resolves in this harness.
      readsStory <- createMessageChecker \case
        StoryMessage (ReadStoryWithPlacement _ c _ _ _) -> toCardCode c == "85022"
        _ -> False

      run $ NextAdvanceActStep act.id 1
      -- resolve the normal AdvanceAct side-A -> side-B choice so the side-B flip
      -- (and its story read) runs.
      chooseOnlyOption "advance the act"

      readsStory `refShouldBe` True
