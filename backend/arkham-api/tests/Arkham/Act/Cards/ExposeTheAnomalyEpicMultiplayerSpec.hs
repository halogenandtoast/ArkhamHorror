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
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher
import Arkham.Projection (field)
import Arkham.ScenarioLogKey (ScenarioCountKey (EpicActAdvances))
import Arkham.Token
import Arkham.Window (defaultWindows)
import TestImport.New

{- | Regression for the Epic Multiplayer variant of Expose the Anomaly (act 85005,
Act 1) in The Blob That Ate Everything, as finalized.

The act now has exactly ONE printed ability:

  * CONTRIBUTION (ability 1, fast, @DuringTurn You@, only while you have clues):
    @chooseAmount@ 1..min(3, spendable clues), then on resolution it moves those
    clues from the investigator physically onto the act and raises the shared
    @act-progress:1@ counter by the amount placed. The clues land on the act via
    the investigator runner's @MoveTokens -> PlaceTokens@ relay (the act's own
    @MoveTokens@ clue clause is a no-op to avoid double counting).

ADVANCE is SEAM-COORDINATED and driven by a server message we run directly:
@ResolveEpicActAdvance 1 spendAmount@. The act's handler (guarded on stage == 1)
computes @leftover = max 0 (clues - spendAmount)@, hands the leftover to this
group's @UneliminatedInvestigator@s split as evenly as possible (the first
@leftover \`mod\` n@ investigators get the +1), increments the LOCAL
@EpicActAdvances 1@ count, advances the act deck directly, and (side effect)
seeds the Vulnerable Heart via @leadChooseOneM@. It emits NO @RaiseShared@ /
@SpendShared@ -- the server owns the pool reset.

The old @SilentForcedAbility@ auto-advance (ability 2) and the
@2 * total * (advances + 1)@ cumulative threshold are GONE; we no longer assert
on @total-investigators@ or any shared-counter threshold.

Harness notes: @advanceActDeck@ is a no-op with no configured act stack, and the
Vulnerable Heart @leadChooseOneM@ has no Oozified locations to target so it is
skipped -- so we assert the load-bearing effects (clue placement, leftover
distribution, @EpicActAdvances@ increment, no Shared* emission) rather than the
deck position.
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

  it "consumes the spend, hands the leftover to the sole investigator, and increments the advance count"
    . scenarioTest "85001"
    $ \self -> do
      act <- realAct Acts.exposeTheAnomalyEpicMultiplayer
      -- 5 clues sitting on the act; the lone investigator starts empty.
      run $ PlaceTokens (TestSource mempty) (toTarget act) Clue 5
      self.clues `shouldReturn` 0
      scenarioCount (EpicActAdvances 1) `shouldReturn` 0

      noShared <- createMessageChecker \case
        RaiseShared {} -> True
        SpendShared {} -> True
        _ -> False

      -- spend 2 of the 5; leftover 3 goes entirely to the single investigator.
      run $ ResolveEpicActAdvance 1 2

      self.clues `shouldReturn` 3
      scenarioCount (EpicActAdvances 1) `shouldReturn` 1
      -- the per-group handler must never touch the shared pool.
      noShared `refShouldBe` False

  it "splits the leftover clues evenly across two investigators on resolve"
    . scenarioTest "85001"
    $ \self -> do
      other <- addInvestigator Investigators.rolandBanks
      act <- realAct Acts.exposeTheAnomalyEpicMultiplayer
      run $ PlaceTokens (TestSource mempty) (toTarget act) Clue 6
      self.clues `shouldReturn` 0
      other.clues `shouldReturn` 0

      -- spend 2 of 6; leftover 4 splits 2/2 (even, so order is irrelevant).
      run $ ResolveEpicActAdvance 1 2

      self.clues `shouldReturn` 2
      other.clues `shouldReturn` 2

  it "gives the odd extra leftover clue to exactly one investigator on resolve"
    . scenarioTest "85001"
    $ \self -> do
      other <- addInvestigator Investigators.rolandBanks
      act <- realAct Acts.exposeTheAnomalyEpicMultiplayer
      run $ PlaceTokens (TestSource mempty) (toTarget act) Clue 7
      self.clues `shouldReturn` 0
      other.clues `shouldReturn` 0

      -- spend 2 of 7; leftover 5 over 2 investigators = base 2 + one +1.
      -- select order is not guaranteed, so assert the multiset {2,3}.
      run $ ResolveEpicActAdvance 1 2

      selfClues <- self.clues
      otherClues <- other.clues
      sort [selfClues, otherClues] `shouldBe` [2, 3]
