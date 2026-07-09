module Arkham.UltimatumsAndBoons.UltimatumOfSurvivalSpec (spec) where

import Helpers.UltimatumsAndBoons
import TestImport.New

spec :: Spec
spec = describe "Ultimatum of Survival" $ do
  context "in a campaign" $ do
    it "a killed investigator's player gets no replacement-deck prompt" . gameTest $ \self -> do
      withUltimatums [UltimatumOfSurvival]
      asCampaign
      updateInvestigator self \attrs ->
        attrs {investigatorKilled = True, investigatorDefeated = True}
      upgradePrompted <- createMessageMatcher UpgradingDecks
      run HandleKilledOrInsaneInvestigators
      upgradePrompted `refShouldBe` False

    it "without the ultimatum the player gets a replacement prompt" . gameTest $ \self -> do
      withUltimatumsDisabled [UltimatumOfSurvival]
      asCampaign
      updateInvestigator self \attrs ->
        attrs {investigatorKilled = True, investigatorDefeated = True}
      upgradePrompted <- createMessageMatcher UpgradingDecks
      run HandleKilledOrInsaneInvestigators
      upgradePrompted `refShouldBe` True
