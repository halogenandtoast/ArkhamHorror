module Arkham.Investigator.Cards.JennyBarnesSpec (
  spec,
) where

import TestImport.Lifted

import Arkham.Classes.HasChaosTokenValue
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Token

spec :: Spec
spec = describe "Jenny Barnes" $ do
  context "ability" $ do
    it "collects 2 resources during the upkeep phase" $ gameTestWith Investigators.jennyBarnes $ \jennyBarnes -> do
      pushAndRun AllDrawCardAndResource
      fieldAssert InvestigatorResources (== 2) jennyBarnes

  context "elder sign token" $ do
    it "modifier is number of resources" $ gameTestWith Investigators.jennyBarnes $ \jennyBarnes -> do
      updateInvestigator jennyBarnes (tokensL %~ setTokens Resource 15)
      token <- getChaosTokenValue (toId jennyBarnes) ElderSign (toId jennyBarnes)
      chaosTokenValue token `shouldBe` Just 15
