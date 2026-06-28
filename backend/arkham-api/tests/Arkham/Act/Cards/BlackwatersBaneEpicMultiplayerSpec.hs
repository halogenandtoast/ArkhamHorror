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
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher
import Arkham.Projection (field)
import Arkham.ScenarioLogKey (ScenarioCountKey (EpicActAdvances, EpicShared))
import Arkham.Token
import Arkham.Window (defaultWindows)
import TestImport.New

{- | Regression for the Epic Multiplayer variant of Blackwater's Bane (act 85008,
Act 3) in The Blob That Ate Everything, as finalized.

Same finalized single-ability shape as the Epic variant of Act 1:

  * CONTRIBUTION (ability 1, fast, @DuringTurn You@, only while you have clues):
    @chooseAmount@ 1..min(3, spendable clues), then moves those clues physically
    onto the act and raises the shared @act-progress:3@ counter by the amount
    placed.

ADVANCE is SEAM-COORDINATED and driven by @ResolveEpicActAdvance 3 spendAmount@,
which we run directly. The act's handler (guarded on stage == 3) computes
@leftover = max 0 (clues - spendAmount)@, hands the leftover to this group's
@UneliminatedInvestigator@s split as evenly as possible (the first
@leftover \`mod\` n@ investigators get the +1), increments the LOCAL
@EpicActAdvances 3@ count, loops the deck back to stage 1, and reads the seeded
Part-1 story. The story pick is deterministic: with @wave = EpicActAdvances 3 + 1@
(read before the increment) and the per-event @blob-story-seed@, the choice is
@(seed + wave) \`mod\` 4@ -> 0 rescueTheChemist / 1 recoverTheSample (85022) /
2 driveOffTheMiGo (85023) / 3 defuseTheExplosives. It emits NO @RaiseShared@ /
@SpendShared@ -- the server owns the pool reset.

The old @SilentForcedAbility@ auto-advance (ability 2) and the
@2 * total * (advances + 1)@ cumulative threshold are GONE.

Harness notes: the heavier side effects are inert here -- no set-aside Mi-Go
Drones to reshuffle, no revealed Oozified locations to seed, and
@ResetActDeckToStage 1@ is a no-op with no configured act stack -- so we assert
the load-bearing effects (clue placement, leftover distribution,
@EpicActAdvances@ increment, the deterministic story read, and no Shared*
emission) rather than the deck position.
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

  it "consumes the spend, hands the leftover to the sole investigator, and increments the advance count"
    . scenarioTest "85001"
    $ \self -> do
      act <- realAct Acts.blackwatersBaneEpicMultiplayer
      run $ PlaceTokens (TestSource mempty) (toTarget act) Clue 5
      self.clues `shouldReturn` 0
      scenarioCount (EpicActAdvances 3) `shouldReturn` 0

      noShared <- createMessageChecker \case
        RaiseShared {} -> True
        SpendShared {} -> True
        _ -> False

      -- spend 2 of the 5; leftover 3 goes entirely to the single investigator.
      run $ ResolveEpicActAdvance 3 2

      self.clues `shouldReturn` 3
      scenarioCount (EpicActAdvances 3) `shouldReturn` 1
      noShared `refShouldBe` False

  it "splits the leftover clues evenly across two investigators on resolve"
    . scenarioTest "85001"
    $ \self -> do
      other <- addInvestigator Investigators.rolandBanks
      act <- realAct Acts.blackwatersBaneEpicMultiplayer
      run $ PlaceTokens (TestSource mempty) (toTarget act) Clue 6
      self.clues `shouldReturn` 0
      other.clues `shouldReturn` 0

      run $ ResolveEpicActAdvance 3 2

      self.clues `shouldReturn` 2
      other.clues `shouldReturn` 2

  it "gives the odd extra leftover clue to exactly one investigator on resolve"
    . scenarioTest "85001"
    $ \self -> do
      other <- addInvestigator Investigators.rolandBanks
      act <- realAct Acts.blackwatersBaneEpicMultiplayer
      run $ PlaceTokens (TestSource mempty) (toTarget act) Clue 7
      self.clues `shouldReturn` 0
      other.clues `shouldReturn` 0

      run $ ResolveEpicActAdvance 3 2

      selfClues <- self.clues
      otherClues <- other.clues
      sort [selfClues, otherClues] `shouldBe` [2, 3]

  it "reads the seed-derived Part-1 story on resolve (seed 0, wave 1 -> Recover the Sample 85022)"
    . scenarioTest "85001"
    $ \_ -> do
      act <- realAct Acts.blackwatersBaneEpicMultiplayer
      run $ ScenarioCountSet (EpicShared "blob-story-seed") 0
      run $ PlaceTokens (TestSource mempty) (toTarget act) Clue 4

      -- wave = EpicActAdvances 3 (0) + 1 = 1; (seed 0 + 1) `mod` 4 = 1 -> 85022.
      -- createMessageChecker fires on dequeue, so the assertion holds regardless
      -- of how far the regenerated story resolves in this harness.
      readsStory <- createMessageChecker \case
        StoryMessage (ReadStoryWithPlacement _ c _ _ _) -> toCardCode c == "85022"
        _ -> False

      run $ ResolveEpicActAdvance 3 2

      readsStory `refShouldBe` True
