-- Scenario shell only: setup, token effects and resolutions are not implemented.
module Arkham.Homebrew.NightOfVespers.Scenarios.TheGardenOfBones (theGardenOfBones) where

import Arkham.Helpers.FlavorText
import Arkham.Homebrew.NightOfVespers.Helpers
import Arkham.Homebrew.NightOfVespers.Intros
import Arkham.Homebrew.NightOfVespers.Jinx (runJinx)
import Arkham.Homebrew.NightOfVespers.Key
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey (ScenarioLogKey (HomebrewScenarioLogKey))

newtype TheGardenOfBones = TheGardenOfBones ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

theGardenOfBones :: Difficulty -> TheGardenOfBones
theGardenOfBones difficulty = scenario TheGardenOfBones ":night-of-vespers:168" "The Garden of Bones" difficulty []

instance RunMessage TheGardenOfBones where
  runMessage msg s@(TheGardenOfBones attrs) =
    runQueueT $ scenarioI18n "theGardenOfBones" $ runJinx msg *> case msg of
      PreScenarioSetup -> do
        readIntro 1
        chooseLead
          [ (2, YouLearnedWhereToFindHelenaNussbaum)
          , (3, YouReadSgtMandelsFile)
          , (4, DrArmitageTaughtYouAboutTheThreeMothers)
          , (5, AnnaKaslowDirectedYouTowardAnOldColleague)
          , (6, MaddalenaGuidesYourPath)
          ]
        pure s
      DoStep 6 PreScenarioSetup -> do
        readIntro 6
        storyWithChooseOneM (h "intro.title" >> p.basic "intro.chooseStart") do
          for_
            [ ("city", "nightOfVespers.startCity")
            , ("mountain", "nightOfVespers.startMountain")
            , ("forest", "nightOfVespers.startForest")
            ]
            \(label, key) ->
              labeled label $ remember $ HomebrewScenarioLogKey key
        pure s
      DoStep n PreScenarioSetup -> do
        readIntro n
        when (n `elem` [2, 3]) $ pEffect ("lead" <> tshow n)
        introStep 6
        pure s
      Setup -> do
        flavor $ scope "rules" $ h "title" >> p "body"
        flavor $ scope "scaffold" $ h "title" >> p "body"
        pure s
      _ -> TheGardenOfBones <$> liftRunMessage msg attrs
