module Arkham.Investigator.Cards.JennyBarnesSpec
  ( spec
  ) where

import TestImport.Lifted
import Arkham.Game ()
import Arkham.Classes.HasTokenValue
import Arkham.Investigator.Types (Field(..))

spec :: Spec
spec = describe "Jenny Barnes" $ do
  context "ability" $ do
    it "collects 2 resources during the upkeep phase" $ do
      let jennyBarnes = lookupInvestigator "02003"
      gameTest jennyBarnes [AllDrawCardAndResource] id $ do
        runMessages
        fieldAssert InvestigatorResources (== 2) jennyBarnes

  context "elder sign token" $ do
    it "modifier is number of resources" $ do
      let jennyBarnes = lookupInvestigator "02003"
      gameTest jennyBarnes [TakeResources (toId jennyBarnes) 5 False] id $ do
        runMessages
        token <- getTokenValue (toId jennyBarnes) ElderSign (toId jennyBarnes)
        tokenValue token `shouldBe` Just 5
