module Arkham.Types.Investigator.Cards.JennyBarnesSpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Token

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
      game <- runGameTest
        jennyBarnes
        [ TakeResources (getId () jennyBarnes) 5 False
        , ResolveToken ElderSign (getId () jennyBarnes)
        ]
        (scenario ?~ scenario')
      game `shouldSatisfy` hasProcessedMessage
        (RunSkillTest (getId () jennyBarnes) [TokenValue ElderSign 5])
