-- Scenario shell only: setup, token effects and resolutions are not implemented.
module Arkham.Homebrew.NightOfVespers.Scenarios.TheGardenOfBones (theGardenOfBones) where

import Arkham.Helpers.FlavorText
import Arkham.Homebrew.NightOfVespers.Helpers
import Arkham.Homebrew.NightOfVespers.Intros
import Arkham.Homebrew.NightOfVespers.Jinx (runJinx)
import Arkham.Scenario.Import.Lifted

newtype TheGardenOfBones = TheGardenOfBones ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

theGardenOfBones :: Difficulty -> TheGardenOfBones
theGardenOfBones difficulty = scenario TheGardenOfBones ":night-of-vespers:168" "The Garden of Bones" difficulty []

instance RunMessage TheGardenOfBones where
  runMessage msg s@(TheGardenOfBones attrs) =
    runQueueT $ scenarioI18n "theGardenOfBones" $ runJinx msg *> case msg of
      PreScenarioSetup -> do
        runScenarioIntro attrs.id 1
        pure s
      DoStep n PreScenarioSetup -> do
        runScenarioIntro attrs.id n
        pure s
      Setup -> do
        flavor $ scope "rules" $ h "title" >> p "body"
        flavor $ scope "scaffold" $ h "title" >> p "body"
        pure s
      _ -> TheGardenOfBones <$> liftRunMessage msg attrs
