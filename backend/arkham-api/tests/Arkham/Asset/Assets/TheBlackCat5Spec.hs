module Arkham.Asset.Assets.TheBlackCat5Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Difficulty
import Arkham.Investigator.Cards (skidsOToole)
import Arkham.Token
import TestImport.New

{- | Swap the harness scenario for the real one, so its own chaos symbol
handlers run. The Vanishing of Elina Harper on Hard is the case from #5352:
passing with [elder_thing] places 1 of your clues on your location.
-}
asScenario :: CardCode -> Difficulty -> TestAppT ()
asScenario code difficulty = do
  overTest \g -> g {gameMode = That (lookupScenario (ScenarioId code) difficulty)}
  tick

spec :: Spec
spec = describe "The Black Cat (5)" do
  -- Regression for #5352.
  context "scenario symbol effects" do
    it "does not place a clue when the elder thing is resolved by The Black Cat"
      . gameTestWith skidsOToole
      $ \self -> do
        asScenario "07056" Hard
        location <- testLocation
        self `moveTo` location
        blackCat <- self `putAssetIntoPlay` Assets.theBlackCat5
        run $ PlaceTokens (TestSource mempty) (toTarget self) Clue 1

        setChaosTokens [ElderThing]
        sid <- getRandom
        -- willpower 2 against difficulty 1, so -1 from The Black Cat still passes
        runSkillTest sid self #willpower 1
        chooseTarget blackCat
        assertPassedSkillTest
        applyResults
        self.clues `shouldReturn` 1

    it "does place a clue when the elder thing is resolved normally"
      . gameTestWith skidsOToole
      $ \self -> do
        asScenario "07056" Hard
        withProp @"willpower" 6 self
        location <- testLocation
        self `moveTo` location
        void $ self `putAssetIntoPlay` Assets.theBlackCat5
        run $ PlaceTokens (TestSource mempty) (toTarget self) Clue 1

        setChaosTokens [ElderThing]
        sid <- getRandom
        -- willpower 6 against difficulty 2, so the elder thing's -4 still passes
        runSkillTest sid self #willpower 2
        clickLabel "$label.resolveNormally"
        assertPassedSkillTest
        applyResults
        self.clues `shouldReturn` 0

  context "elder sign" do
    it "replaces the symbol's normal effects, not just its value" . gameTestWith skidsOToole $ \self -> do
      -- Skids' own elder sign is "+2. Gain 2 resources", so resolving with The
      -- Black Cat must give +5 instead and skip the resource gain entirely.
      blackCat <- self `putAssetIntoPlay` Assets.theBlackCat5
      withProp @"resources" 0 self

      setChaosTokens [ElderSign]
      sid <- getRandom
      -- willpower 2 against difficulty 6: fails at +2, passes at +5
      runSkillTest sid self #willpower 6
      chooseTarget blackCat
      assertPassedSkillTest
      applyResults
      self.resources `shouldReturn` 0

    it "leaves Skids' elder sign alone when resolved normally" . gameTestWith skidsOToole $ \self -> do
      void $ self `putAssetIntoPlay` Assets.theBlackCat5
      withProp @"resources" 0 self

      setChaosTokens [ElderSign]
      sid <- getRandom
      runSkillTest sid self #willpower 4
      clickLabel "$label.resolveNormally"
      assertPassedSkillTest
      applyResults
      self.resources `shouldReturn` 2

  context "elder thing" do
    it "deals 1 direct horror to The Black Cat" . gameTestWith skidsOToole $ \self -> do
      blackCat <- self `putAssetIntoPlay` Assets.theBlackCat5

      setChaosTokens [ElderThing]
      sid <- getRandom
      runSkillTest sid self #willpower 2
      chooseTarget blackCat
      applyResults
      blackCat.horror `shouldReturn` 1
      blackCat.damage `shouldReturn` 0

  context "tablet" do
    it "deals 1 direct damage to The Black Cat" . gameTestWith skidsOToole $ \self -> do
      blackCat <- self `putAssetIntoPlay` Assets.theBlackCat5

      setChaosTokens [Tablet]
      sid <- getRandom
      runSkillTest sid self #willpower 2
      chooseTarget blackCat
      applyResults
      blackCat.damage `shouldReturn` 1
      blackCat.horror `shouldReturn` 0
