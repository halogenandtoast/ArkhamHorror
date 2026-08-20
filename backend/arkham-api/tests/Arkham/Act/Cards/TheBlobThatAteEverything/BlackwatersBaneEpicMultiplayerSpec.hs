module Arkham.Act.Cards.TheBlobThatAteEverything.BlackwatersBaneEpicMultiplayerSpec (spec) where

import Arkham.Ability.Types (Ability, abilityIndex, abilitySource)
import Arkham.Act (lookupAct)
import Arkham.Act.CardDefs.TheBlobThatAteEverything qualified as Acts
import Arkham.Act.Sequence (ActSide (..))
import Arkham.Act.Types (Act)
import Arkham.Entities qualified as Entities
import Arkham.Helpers.Action (getActions)
import Arkham.Helpers.Log (scenarioCount)
import Arkham.Matcher
import Arkham.ScenarioLogKey (ScenarioCountKey (EpicActAdvances, EpicShared))
import Arkham.Window (defaultWindows, mkWhen)
import Arkham.Window qualified as Window
import TestImport.New

{- | Regression coverage for the printed Epic Multiplayer Blackwater's Bane
(85008). This act is NOT a global-clue act: after its moment of respite it must
advance at the end of the round. Its Epic-only behavior is selecting the same
seeded Part-1 story in every group for a given Act 3 cycle.
-}
realAct :: CardDef -> TestAppT Act
realAct def = do
  card <- genCard def
  let actId' = ActId (toCardCode card)
      act' = either (error . show) id $ lookupAct actId' 1 (toCardId card)
  overTest $ entitiesL . Entities.actsL %~ insertEntity act'
  pure act'

endRoundObjective :: Investigator -> Act -> TestAppT (Maybe Ability)
endRoundObjective self act = do
  let ws = [mkWhen Window.AtEndOfRound]
  find (\ab -> abilitySource ab == toSource act && abilityIndex ab == 1)
    <$> getActions (toId self) ws

spec :: Spec
spec = describe "Blackwater's Bane (Epic Multiplayer)" do
  it "has no clue-contribution ability" . scenarioTest "85001" $ \self -> do
    act <- realAct Acts.blackwatersBaneEpicMultiplayer
    abilities <- getActions (toId self) (defaultWindows $ toId self)
    filter (\ab -> abilitySource ab == toSource act) abilities `shouldBe` []

  it "does not advance during the round it enters play" . scenarioTest "85001" $ \self -> do
    act <- realAct Acts.blackwatersBaneEpicMultiplayer
    endRoundObjective self act `shouldReturn` Nothing
    assertAny $ ActWithSide A

  it "must advance at the end of a later round" . scenarioTest "85001" $ \self -> do
    act <- realAct Acts.blackwatersBaneEpicMultiplayer
    run $ Do BeginRound
    ability <- endRoundObjective self act
    case ability of
      Nothing -> liftIO $ expectationFailure "expected the end-of-round objective"
      Just ab -> run $ UseAbility (toId self) ab [mkWhen Window.AtEndOfRound]
    assertAny $ ActWithSide B
    assertNone $ ActWithSide A

  it "reads the same seed-derived Part-1 story for the cycle" . scenarioTest "85001" $ \_ -> do
    act <- realAct Acts.blackwatersBaneEpicMultiplayer
    run $ ScenarioCountSet (EpicShared "blob-story-seed") 0

    readsStory <- createMessageChecker \case
      StoryMessage (ReadStoryWithPlacement _ c _ _ _) -> toCardCode c == "85022"
      _ -> False

    -- Advancing reads the story immediately; the story itself may then park on
    -- one of several investigator choices, so there is no single generic option
    -- for this test to answer.
    run $ NextAdvanceActStep act.id 1

    scenarioCount (EpicActAdvances 3) `shouldReturn` 1
    -- NextAdvanceActStep records the cycle and opens the normal act-advance
    -- confirmation. At this point that confirmation is the sole choice; the
    -- story's own multi-choice question is opened only after answering it.
    chooseOnlyOption "advance the act"
    readsStory `refShouldBe` True
