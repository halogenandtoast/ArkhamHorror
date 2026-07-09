module Arkham.UltimatumsAndBoons.UltimatumOfDreadSpec (spec) where

import Arkham.Phase
import Helpers.UltimatumsAndBoons
import TestImport.New

-- The harness game never runs Setup, but EndSetup is the message that decides
-- which phase round 1 begins with, so we can drive it directly.
spec :: Spec
spec = describe "Ultimatum of Dread" $ do
  it "does not skip the mythos phase during the first round" . gameTest $ \_ -> do
    withUltimatums [UltimatumOfDread]
    beganMythos <- createMessageMatcher (Begin MythosPhase)
    run EndSetup
    beganMythos `refShouldBe` True

  it "has no effect while ultimatums and boons are disabled" . gameTest $ \_ -> do
    withUltimatumsDisabled [UltimatumOfDread]
    beganMythos <- createMessageMatcher (Begin MythosPhase)
    beganInvestigation <- createMessageMatcher (Begin InvestigationPhase)
    run EndSetup
    beganMythos `refShouldBe` False
    beganInvestigation `refShouldBe` True
