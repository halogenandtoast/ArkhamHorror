module Arkham.Homebrew.NightOfVespers.CampaignSpec (spec) where

import Arkham.Campaign.Types
import Arkham.CampaignStep
import Arkham.Difficulty (Difficulty (Standard))
import Arkham.Homebrew.NightOfVespers.Campaign
import Arkham.Homebrew.NightOfVespers.CampaignSteps
import Arkham.Scenario.Options
import TestImport

spec :: Spec
spec = describe "Night of Vespers campaign progression" do
  let initial = toAttrs $ nightOfVespers Standard
      campaignAt step completed = NightOfVespers $ initial {campaignStep = step, campaignCompletedSteps = completed}
  it "begins with The End of Seasons" do
    nextStep (campaignAt PrologueStep []) `shouldBe` continue TheEndOfSeasons
  it "plays Dancing with Death after Behind Closed Doors when it has not been played" do
    nextStep (campaignAt BehindClosedDoors []) `shouldBe` continue DancingWithDeath
  it "plays Behind Closed Doors after Dancing with Death when it has not been played" do
    nextStep (campaignAt DancingWithDeath []) `shouldBe` continue BehindClosedDoors
  it "leaves chapter II after both scenarios, in either order" do
    nextStep (campaignAt BehindClosedDoors [DancingWithDeath]) `shouldBe` continue TheRoseOfTuscany
    nextStep (campaignAt DancingWithDeath [BehindClosedDoors]) `shouldBe` continue TheRoseOfTuscany
  it "handles a scenario started with lead-investigator options" do
    nextStep
      ( campaignAt
          (ScenarioStepWithOptions ":night-of-vespers:037" defaultScenarioOptions)
          [DancingWithDeath]
      )
      `shouldBe` continue TheRoseOfTuscany
  it "offers onward travel after the first and second chapter IV destinations" do
    nextStep (campaignAt TheCityOfFog []) `shouldBe` continue ChooseDestination
    nextStep (campaignAt TheGardenOfBones [TheCityOfFog]) `shouldBe` continue ChooseDestination
  it "counts the current destination when ending chapter IV, in every order" do
    for_
      [ (TheCityOfFog, [TheGardenOfBones, AChorusOfSuffering])
      , (TheGardenOfBones, [TheCityOfFog, AChorusOfSuffering])
      , (AChorusOfSuffering, [TheCityOfFog, TheGardenOfBones])
      ]
      \(step, completed) ->
        nextStep (campaignAt step completed) `shouldBe` continue OurLadiesOfSorrow
  it "ends after the final scenario and epilogue" do
    nextStep (campaignAt MothersMercy []) `shouldBe` continue EpilogueStep
    nextStep (campaignAt EpilogueStep []) `shouldBe` Nothing
