-- Scenario shell only: setup, token effects and resolutions are not implemented.
module Arkham.Homebrew.NightOfVespers.Scenarios.DancingWithDeath (dancingWithDeath) where

import Arkham.Helpers.FlavorText
import Arkham.Homebrew.NightOfVespers.Helpers
import Arkham.Homebrew.NightOfVespers.Intros
import Arkham.Homebrew.NightOfVespers.Jinx (runJinx)
import Arkham.Homebrew.NightOfVespers.Key
import Arkham.Scenario.Import.Lifted

newtype DancingWithDeath = DancingWithDeath ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

dancingWithDeath :: Difficulty -> DancingWithDeath
dancingWithDeath difficulty = scenario DancingWithDeath ":night-of-vespers:075" "Dancing with Death" difficulty []

instance RunMessage DancingWithDeath where
  runMessage msg s@(DancingWithDeath attrs) =
    runQueueT $ scenarioI18n "dancingWithDeath" $ runJinx msg *> case msg of
      PreScenarioSetup -> do
        readIntro 1
        gigi <- getHasRecord GigiGiulianoIsAssistingTheInvestigation
        let branch = if gigi then 2 else 3
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
      _ -> DancingWithDeath <$> liftRunMessage msg attrs
