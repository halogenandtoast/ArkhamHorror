module Arkham.Types.Investigator.Cards.JennyBarnesSpec
  ( spec
  )
where

import TestImport.Lifted

spec :: Spec
spec = describe "Jenny Barnes" $ do
  context "ability" $ do
    it "collects 2 resources during the upkeep phase" $ do
      let jennyBarnes = lookupInvestigator "02003"
      gameTest jennyBarnes [AllDrawCardAndResource] id $ do
        runMessages
        updatedResourceCount jennyBarnes `shouldReturn` 2

  context "elder sign token" $ do
    it "modifier is number of resources" $ do
      let jennyBarnes = lookupInvestigator "02003"
      gameTest jennyBarnes [TakeResources (toId jennyBarnes) 5 False] id $ do
        runMessages
        jennyBarnes' <- updated jennyBarnes
        token <- getTokenValue jennyBarnes' (toId jennyBarnes') ElderSign
        tokenValue token `shouldBe` Just 5
