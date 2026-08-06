module Arkham.Scenario.Scenarios.HemlockValeStandaloneSpec (spec) where

import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Helpers.Scenario (getIsStandalone)
import TestImport.New

{- | Standalone play has no @Campaign@ entity, so the Hemlock Vale day/time
helpers used to blow up on @selectJust TheCampaign@ (#5349). They now fall back
to whatever the scenario stashed during @PreScenarioSetup@.
-}
spec :: Spec
spec = describe "The Feast of Hemlock Vale standalone" do
  it "reports standalone when there is no campaign" . scenarioTest "10626" $ \_ ->
    getIsStandalone `shouldReturn` True

  it "does not throw reading the day/time without a campaign" . scenarioTest "10626" $ \_ -> do
    getCampaignDay `shouldReturn` Day1
    getCampaignTime `shouldReturn` Day

  it "uses The Longest Night's fixed Day 2 / Night once PreScenarioSetup has run"
    . scenarioTest "10626"
    $ \_ -> do
      pushAndRun PreScenarioSetup
      getCampaignDay `shouldReturn` Day2
      getCampaignTime `shouldReturn` Night

  it "rolls a day for a survey scenario" . scenarioTest "10523" $ \_ -> do
    pushAndRun PreScenarioSetup
    day <- getCampaignDay
    liftIO $ day `shouldSatisfy` (`elem` [Day1, Day2, Day3])

  describe "the shared survey standalone chaos bag" do
    let sizes = map (length . hemlockStandaloneBag) [Day1, Day2, Day3]
    let counts face = map (length . filter (== face) . hemlockStandaloneBag) [Day1, Day2, Day3]

    it "grows from 16 to 20 tokens across the three days" do
      sizes `shouldBe` [16, 18, 20] :: IO ()

    it "scales the tablets with the day" do
      counts Tablet `shouldBe` [1, 2, 3] :: IO ()

    it "scales the elder things with the day" do
      counts ElderThing `shouldBe` [1, 2, 3] :: IO ()

    it "always carries an elder sign and an auto-fail" do
      counts ElderSign `shouldBe` [1, 1, 1] :: IO ()
      counts AutoFail `shouldBe` [1, 1, 1] :: IO ()
