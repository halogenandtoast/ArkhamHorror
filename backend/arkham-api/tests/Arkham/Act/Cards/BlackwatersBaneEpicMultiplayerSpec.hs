module Arkham.Act.Cards.BlackwatersBaneEpicMultiplayerSpec (spec) where

import Arkham.Act (lookupAct)
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence (ActSide (..))
import Arkham.Act.Types (Act)
import Arkham.Ability.Types (abilityIndex, abilitySource)
import Arkham.Entities qualified as Entities
import Arkham.Epic.Types (SharedKey (..))
import Arkham.Helpers.Action (getActions)
import Arkham.Helpers.Log (scenarioCount)
import Arkham.Matcher
import Arkham.ScenarioLogKey (ScenarioCountKey (EpicActAdvances, EpicShared))
import Arkham.Token
import Arkham.Window (mkWhen)
import Arkham.Window qualified as Window
import TestImport.New

{- | Regression for the Epic Multiplayer variant of Blackwater's Bane (act 85008,
Act 3) in The Blob That Ate Everything.

Same shared-pool mechanics as the Epic variant of Act 1: each group contributes
2 clues per local player into the shared @act-progress:3@ counter at round start
(ability 1), and a silent forced ability (ability 2) keyed on the shared-counter
mirror auto-advances the act in every group once the pooled total reaches the
threshold for the NEXT advance.

The threshold is CUMULATIVE rather than a flat @2 * total@. The shared pool is
never reset; each group tracks how many times THIS act has already advanced in a
LOCAL (non-shared) @EpicActAdvances 3@ scenario count, and the (advances + 1)th
advance fires once the pool reaches @2 * total * (advances + 1)@. With total = 2
the first advance needs 4 pooled clues and the second needs 8. Advancing
increments @EpicActAdvances 3@ in the act's B-side handler, which only runs after
the single-option advance prompt is answered.

The shared seam is simulated as in
'Arkham.Enemy.Cards.Subject8L08EpicMultiplayerSpec': @ScenarioCountSet (EpicShared ...)@
mirrors the authoritative copy into this group's scenario state; setting
@act-progress:3@ fires the @ScenarioCountIncremented@ window which auto-resolves
ability 2 with no player prompt.

Note: Act 3's B-side carries heavier side-effects than Act 1 (set-aside Mi-Go
Drone reshuffle, Oozified-location clue seeding, a set-aside story read, and a
@ResetActDeckToStage 1@). In this minimal harness none of those inputs are
present (no set-aside cards, no Oozified/revealed locations, no completed act
stack), so they are inert no-ops and the advance prompt resolves cleanly,
letting us assert the @EpicActAdvances 3@ increment directly (see the third case).
-}
realAct :: CardDef -> TestAppT Act
realAct def = do
  card <- genCard def
  let actId' = ActId (toCardCode card)
      act' = either (error . show) id $ lookupAct actId' 1 (toCardId card)
  overTest $ entitiesL . Entities.actsL %~ insertEntity act'
  pure act'

spec :: Spec
spec = describe "Blackwater's Bane (Epic Multiplayer)" do
  it "does not advance while the pooled clue total is below the first threshold"
    . scenarioTest "85001"
    $ \_ -> do
      act <- realAct Acts.blackwatersBaneEpicMultiplayer
      -- frozen total = 2 investigators => first threshold = 2 * 2 * 1 = 4
      run $ ScenarioCountSet (EpicShared "total-investigators") 2

      advanced <- createMessageChecker \case
        AdvanceAct aid _ _ -> aid == act.id
        _ -> False

      -- 3 < 4: ability 2 fires but the act stays on side A
      run $ ScenarioCountSet (EpicShared "act-progress:3") 3
      advanced `refShouldBe` False
      assertAny $ ActWithSide A

  it "auto-advances once the pooled clue total reaches the first threshold"
    . scenarioTest "85001"
    $ \_ -> do
      act <- realAct Acts.blackwatersBaneEpicMultiplayer
      -- frozen total = 2 investigators => first threshold = 2 * 2 * 1 = 4
      run $ ScenarioCountSet (EpicShared "total-investigators") 2

      advanced <- createMessageChecker \case
        AdvanceAct aid _ _ -> aid == act.id
        _ -> False

      run $ ScenarioCountSet (EpicShared "act-progress:3") 4
      advanced `refShouldBe` True

  it "records the advance in the local EpicActAdvances count when it advances"
    . scenarioTest "85001"
    $ \_ -> do
      act <- realAct Acts.blackwatersBaneEpicMultiplayer
      -- frozen total = 2 investigators => first threshold = 2 * 2 * 1 = 4
      run $ ScenarioCountSet (EpicShared "total-investigators") 2

      advanced <- createMessageChecker \case
        AdvanceAct aid _ _ -> aid == act.id
        _ -> False

      -- this group has advanced 0 times so far
      scenarioCount (EpicActAdvances 3) `shouldReturn` 0

      -- reaching the threshold flips the act to side B and queues the
      -- single-option advance confirmation; advancedWithOther emits the first
      -- AdvanceAct before that prompt, so the checker fires now.
      run $ ScenarioCountSet (EpicShared "act-progress:3") 4
      advanced `refShouldBe` True

      -- the act's B-side handler (which increments EpicActAdvances) only runs
      -- once the single-option advance prompt is answered. The heavier B-side
      -- side-effects are inert in this harness (see module note).
      chooseOnlyOption "advance act"
      scenarioCount (EpicActAdvances 3) `shouldReturn` 1

  it "needs another 2*total clues for the second pass once it has advanced once"
    . scenarioTest "85001"
    $ \_ -> do
      act <- realAct Acts.blackwatersBaneEpicMultiplayer
      -- frozen total = 2 investigators; seed one prior advance directly so the
      -- next threshold is 2 * 2 * (1 + 1) = 8 (the second pass), without having
      -- to loop the act deck back round.
      run $ ScenarioCountSet (EpicShared "total-investigators") 2
      run $ ScenarioCountSet (EpicActAdvances 3) 1

      advanced <- createMessageChecker \case
        AdvanceAct aid _ _ -> aid == act.id
        _ -> False

      -- 4 was the FIRST threshold; with advances = 1 it must NOT re-advance.
      run $ ScenarioCountSet (EpicShared "act-progress:3") 4
      advanced `refShouldBe` False
      assertAny $ ActWithSide A

      -- 8 = 2 * total * 2 reaches the second-pass threshold and advances.
      run $ ScenarioCountSet (EpicShared "act-progress:3") 8
      advanced `refShouldBe` True

  it "reads the seed-derived Part-1 story on the first 3b (seed 0, wave 1 -> Recover the Sample)"
    . scenarioTest "85001"
    $ \_ -> do
      -- Register the act so its silent auto-advance ability (2) can fire; we observe
      -- the story read rather than the AdvanceAct, so no `act` reference is needed.
      _ <- realAct Acts.blackwatersBaneEpicMultiplayer
      -- seed 0, frozen total = 2 => first threshold = 2 * 2 * 1 = 4. This group has
      -- advanced 0 times, so the B-side reads `wave = EpicActAdvances 3 + 1 = 1`
      -- BEFORE the queued increment, and the deterministic Part-1 pick is
      -- `(seed + wave) `mod` 4 = (0 + 1) `mod` 4 = 1` -> 85022 (Recover the Sample).
      run $ ScenarioCountSet (EpicShared "blob-story-seed") 0
      run $ ScenarioCountSet (EpicShared "total-investigators") 2

      -- Set the checker up BEFORE resolving the advance so it captures the
      -- StoryMessage when the B-side handler pushes it. createMessageChecker fires
      -- on dequeue (before the message is handled), so the assertion holds even if
      -- fully resolving the regenerated story were to do nothing useful here.
      readsStory <- createMessageChecker \case
        StoryMessage (ReadStoryWithPlacement _ c _ _ _) -> toCardCode c == "85022"
        _ -> False

      -- Reaching the first threshold flips the act to side B and queues the
      -- single-option advance confirmation.
      run $ ScenarioCountSet (EpicShared "act-progress:3") 4

      -- The B-side handler (which reads the shared Part-1 story via
      -- readStoryWithPlacement_) only runs once the single-option advance prompt is
      -- answered.
      chooseOnlyOption "advance act"
      readsStory `refShouldBe` True

  it "re-rolls the Part-1 story on the second 3b (seed 0, wave 2 -> Drive Off the Mi-Go)"
    . scenarioTest "85001"
    $ \_ -> do
      _ <- realAct Acts.blackwatersBaneEpicMultiplayer
      -- Seed one prior advance directly so this is the SECOND pass: with
      -- EpicActAdvances 3 = 1 the B-side reads `wave = 1 + 1 = 2`, and the
      -- cumulative threshold is `2 * total * (advances + 1) = 2 * 2 * 2 = 8`. The
      -- deterministic Part-1 pick is `(seed + wave) `mod` 4 = (0 + 2) `mod` 4 = 2`
      -- -> 85023 (Drive Off the Mi-Go). Set the seed/total/advances counts BEFORE the
      -- act-progress set that triggers the advance.
      run $ ScenarioCountSet (EpicShared "blob-story-seed") 0
      run $ ScenarioCountSet (EpicShared "total-investigators") 2
      run $ ScenarioCountSet (EpicActAdvances 3) 1

      readsStory <- createMessageChecker \case
        StoryMessage (ReadStoryWithPlacement _ c _ _ _) -> toCardCode c == "85023"
        _ -> False

      -- 8 = 2 * total * (1 + 1) reaches the second-pass threshold and advances.
      run $ ScenarioCountSet (EpicShared "act-progress:3") 8

      chooseOnlyOption "advance act"
      readsStory `refShouldBe` True

  it "contributes this group's share to the shared pool without advancing"
    . scenarioTest "85001"
    $ \self -> do
      act <- realAct Acts.blackwatersBaneEpicMultiplayer
      loc <- testLocation
      self `moveTo` loc
      run $ PlaceTokens (TestSource mempty) (toTarget self) Clue 2

      raised <- createMessageChecker \case
        RaiseShared (SharedActProgress 3) n -> n == 2
        _ -> False

      let roundBeginsWindows = [mkWhen Window.AtBeginningOfRound]
      abilities <-
        filter (\ab -> abilitySource ab == toSource act && abilityIndex ab == 1)
          <$> getActions (toId self) roundBeginsWindows
      case abilities of
        (ability : _) -> run $ UseAbility (toId self) ability roundBeginsWindows
        [] ->
          liftIO
            $ expectationFailure
              "expected Blackwater's Bane's round-start contribution ability to be available"

      raised `refShouldBe` True
      assertAny $ ActWithSide A
