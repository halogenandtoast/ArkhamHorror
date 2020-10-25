module Arkham.Types.Investigator.Cards.JennyBarnesSpec
  ( spec
  )
where

import TestImport

spec :: Spec
spec = describe "Jenny Barnes" $ do
  context "ability" $ do
    it "collects 2 resources during the upkeep phase" $ do
      let jennyBarnes = lookupInvestigator "02003"
      game <- runGameTest jennyBarnes [AllDrawCardAndResource] id
      updatedResourceCount game jennyBarnes `shouldBe` 2
  context "elder sign token" $ do
    it "modifier is number of resources" $ do
      let jennyBarnes = lookupInvestigator "02003"
      scenario' <- testScenario "00000" id
      elderSign <- flip DrawnToken ElderSign . TokenId <$> liftIO nextRandom
      game <- runGameTest
        jennyBarnes
        [TakeResources (getId () jennyBarnes) 5 False]
        (scenario ?~ scenario')
      token <- withGame game $ getTokenValue
        (updated game jennyBarnes)
        (getId () jennyBarnes)
        elderSign
      tokenValue token `shouldBe` Just 5
