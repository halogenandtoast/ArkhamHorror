module Arkham.Act.Cards.ExposeTheAnomalyEpicMultiplayerSpec (spec) where

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

{- | Regression for the Epic Multiplayer variant of Expose the Anomaly (act 85005,
Act 1) in The Blob That Ate Everything.

In Epic Multiplayer the act's clue requirement is a single GLOBAL pool shared
across every group: 2 clues per investigator across ALL groups (the event's
frozen total). Each group contributes its local share into the shared
@act-progress:1@ counter at round start (ability 1); a silent forced ability
(ability 2) keyed on the shared-counter mirror auto-advances the act in every
group once the pooled total reaches the threshold for the NEXT advance.

The threshold is CUMULATIVE rather than a flat @2 * total@. The shared pool is
never reset; instead each group tracks how many times THIS act has already
advanced in a LOCAL (non-shared) @EpicActAdvances 1@ scenario count, and the
(advances + 1)th advance fires once the pool reaches @2 * total * (advances + 1)@.
So with total = 2 the first advance needs 4 pooled clues, the second needs 8,
and so on. Advancing increments @EpicActAdvances 1@ in the act's B-side handler,
which only runs after the single-option advance prompt is answered.

The shared seam is simulated exactly as in
'Arkham.Enemy.Cards.Subject8L08EpicMultiplayerSpec': the authoritative copy is
mirrored into this group's scenario state with @ScenarioCountSet (EpicShared ...)@.
Setting @act-progress:1@ fires the @ScenarioCountIncremented@ window, which
auto-resolves the act's silent forced ability with no player prompt. The emitted
@RaiseShared@ delta is inert in this harness but still observable via a message
checker.
-}
realAct :: CardDef -> TestAppT Act
realAct def = do
  card <- genCard def
  let actId' = ActId (toCardCode card)
      act' = either (error . show) id $ lookupAct actId' 1 (toCardId card)
  overTest $ entitiesL . Entities.actsL %~ insertEntity act'
  pure act'

spec :: Spec
spec = describe "Expose the Anomaly (Epic Multiplayer)" do
  it "does not advance while the pooled clue total is below the first threshold"
    . scenarioTest "85001"
    $ \_ -> do
      act <- realAct Acts.exposeTheAnomalyEpicMultiplayer
      -- frozen total = 2 investigators => first threshold = 2 * 2 * 1 = 4
      run $ ScenarioCountSet (EpicShared "total-investigators") 2

      advanced <- createMessageChecker \case
        AdvanceAct aid _ _ -> aid == act.id
        _ -> False

      -- 3 < 4: ability 2 fires but the act stays on side A
      run $ ScenarioCountSet (EpicShared "act-progress:1") 3
      advanced `refShouldBe` False
      assertAny $ ActWithSide A

  it "auto-advances once the pooled clue total reaches the first threshold"
    . scenarioTest "85001"
    $ \_ -> do
      act <- realAct Acts.exposeTheAnomalyEpicMultiplayer
      -- frozen total = 2 investigators => first threshold = 2 * 2 * 1 = 4
      run $ ScenarioCountSet (EpicShared "total-investigators") 2

      advanced <- createMessageChecker \case
        AdvanceAct aid _ _ -> aid == act.id
        _ -> False

      -- mirroring the pooled progress up to the threshold fires the
      -- ScenarioCountIncremented window -> silent forced ability 2 -> advance
      run $ ScenarioCountSet (EpicShared "act-progress:1") 4
      advanced `refShouldBe` True

  it "records the advance in the local EpicActAdvances count when it advances"
    . scenarioTest "85001"
    $ \_ -> do
      act <- realAct Acts.exposeTheAnomalyEpicMultiplayer
      -- frozen total = 2 investigators => first threshold = 2 * 2 * 1 = 4
      run $ ScenarioCountSet (EpicShared "total-investigators") 2

      advanced <- createMessageChecker \case
        AdvanceAct aid _ _ -> aid == act.id
        _ -> False

      -- this group has advanced 0 times so far
      scenarioCount (EpicActAdvances 1) `shouldReturn` 0

      -- reaching the threshold flips the act to side B and queues the
      -- single-option advance confirmation (advanceActSideA); advancedWithOther
      -- emits the first AdvanceAct before that prompt, so the checker fires now.
      run $ ScenarioCountSet (EpicShared "act-progress:1") 4
      advanced `refShouldBe` True

      -- the act's B-side handler (which increments EpicActAdvances) only runs
      -- once the single-option advance prompt is answered.
      chooseOnlyOption "advance act"
      scenarioCount (EpicActAdvances 1) `shouldReturn` 1

  it "needs another 2*total clues for the second pass once it has advanced once"
    . scenarioTest "85001"
    $ \_ -> do
      act <- realAct Acts.exposeTheAnomalyEpicMultiplayer
      -- frozen total = 2 investigators; seed one prior advance directly so the
      -- next threshold is 2 * 2 * (1 + 1) = 8 (the second pass), without having
      -- to loop the act deck back round.
      run $ ScenarioCountSet (EpicShared "total-investigators") 2
      run $ ScenarioCountSet (EpicActAdvances 1) 1

      advanced <- createMessageChecker \case
        AdvanceAct aid _ _ -> aid == act.id
        _ -> False

      -- 4 was the FIRST threshold; with advances = 1 it must NOT re-advance.
      run $ ScenarioCountSet (EpicShared "act-progress:1") 4
      advanced `refShouldBe` False
      assertAny $ ActWithSide A

      -- 8 = 2 * total * 2 reaches the second-pass threshold and advances.
      run $ ScenarioCountSet (EpicShared "act-progress:1") 8
      advanced `refShouldBe` True

  it "contributes this group's share to the shared pool without advancing"
    . scenarioTest "85001"
    $ \self -> do
      act <- realAct Acts.exposeTheAnomalyEpicMultiplayer
      -- GroupClueCost (PerPlayer 2) requires payers AT a location; one
      -- investigator with exactly 2 clues = the exact (PerPlayer 2) cost.
      loc <- testLocation
      self `moveTo` loc
      run $ PlaceTokens (TestSource mempty) (toTarget self) Clue 2

      raised <- createMessageChecker \case
        RaiseShared (SharedActProgress 1) n -> n == 2
        _ -> False

      -- ability 1 is triggered by RoundBegins #when; surface it under that window
      let roundBeginsWindows = [mkWhen Window.AtBeginningOfRound]
      abilities <-
        filter (\ab -> abilitySource ab == toSource act && abilityIndex ab == 1)
          <$> getActions (toId self) roundBeginsWindows
      case abilities of
        (ability : _) -> run $ UseAbility (toId self) ability roundBeginsWindows
        [] ->
          liftIO
            $ expectationFailure
              "expected Expose the Anomaly's round-start contribution ability to be available"

      -- perPlayer 2 = 2 for the single seeded investigator: the contribution
      -- feeds the shared pool and must NOT advance this group's act.
      raised `refShouldBe` True
      assertAny $ ActWithSide A
