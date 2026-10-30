-- Scenario shell only: setup, token effects and resolutions are not implemented.
module Arkham.Homebrew.NightOfVespers.Scenarios.BehindClosedDoors (behindClosedDoors) where

import Arkham.Helpers.FlavorText
import Arkham.Homebrew.NightOfVespers.Helpers
import Arkham.Homebrew.NightOfVespers.Intros
import Arkham.Homebrew.NightOfVespers.Jinx (runJinx)
import Arkham.Homebrew.NightOfVespers.Key
import Arkham.Scenario.Import.Lifted

newtype BehindClosedDoors = BehindClosedDoors ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

behindClosedDoors :: Difficulty -> BehindClosedDoors
behindClosedDoors difficulty = scenario BehindClosedDoors ":night-of-vespers:037" "Behind Closed Doors" difficulty []

instance RunMessage BehindClosedDoors where
  runMessage msg s@(BehindClosedDoors attrs) =
    runQueueT $ scenarioI18n "behindClosedDoors" $ runJinx msg *> case msg of
      PreScenarioSetup -> do
        readIntro 1
        finlay <- getHasRecord FinlayODoyleIsAssistingTheInvestigation
        let branch = if finlay then 2 else 3
        rememberIntro branch
        introStep branch
        pure s
      DoStep n PreScenarioSetup -> do
        readIntro n
        pure s
      Setup -> do
        flavor $ scope "rules" $ h "title" >> p "body"
        flavor $ scope "scaffold" $ h "title" >> p "body"
        pure s
      _ -> BehindClosedDoors <$> liftRunMessage msg attrs
