module Arkham.UltimatumsAndBoons.UltimatumOfFailureSpec (spec) where

import Arkham.ChaosBag.Base
import Arkham.Helpers.Scenario
import Arkham.Scenario.Types (Field (..))
import Helpers.UltimatumsAndBoons
import TestImport.New

spec :: Spec
spec = describe "Ultimatum of Failure" $ do
  it "adds 1 additional autofail token when the chaos bag is built" . gameTest $ \_ -> do
    withUltimatums [UltimatumOfFailure]
    setChaosTokens [Zero]
    tokens <- scenarioFieldMap ScenarioChaosBag (map chaosTokenFace . chaosBagChaosTokens)
    tokens `shouldMatchList` [Zero, AutoFail]

  it "has no effect while ultimatums and boons are disabled" . gameTest $ \_ -> do
    withUltimatumsDisabled [UltimatumOfFailure]
    setChaosTokens [Zero]
    tokens <- scenarioFieldMap ScenarioChaosBag (map chaosTokenFace . chaosBagChaosTokens)
    tokens `shouldMatchList` [Zero]
