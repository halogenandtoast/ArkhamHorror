module Arkham.UltimatumsAndBoons.UltimatumOfMalevolenceSpec (spec) where

import Arkham.Classes.HasGame (getGame)
import Arkham.Difficulty
import Arkham.Helpers.Scenario (isEasyStandard, isHardExpert)
import Arkham.Scenario.Types (ScenarioAttrs (..))
import TestImport.New

-- NOTE: the real path sets 'scenarioUseHardExpertReference' inside the
-- StartScenario handler (Game/Runner), which rebuilds the scenario from
-- scratch and can't be driven by the harness. This covers the reference-side
-- flip at the unit level instead: the flag makes isHardExpert/isEasyStandard
-- report Hard/Expert while the printed difficulty stays Easy.
spec :: Spec
spec = describe "Ultimatum of Malevolence" $ do
  it "flips the reference card to hard/expert without changing the difficulty" . gameTest $ \_ -> do
    scenario' <-
      fromJustNote "test harness always has a scenario" . modeScenario . gameMode <$> getGame
    -- the harness runs Easy, which normally reads the easy/standard side
    let attrs = toAttrs scenario'
    liftIO $ do
      scenarioDifficulty attrs `shouldBe` Easy
      isEasyStandard attrs `shouldBe` True
      isHardExpert attrs `shouldBe` False

    let flipped = toAttrs $ overAttrs (\a -> a {scenarioUseHardExpertReference = True}) scenario'
    liftIO $ do
      scenarioDifficulty flipped `shouldBe` Easy
      isEasyStandard flipped `shouldBe` False
      isHardExpert flipped `shouldBe` True
