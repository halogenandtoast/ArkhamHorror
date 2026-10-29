-- Scenario shell only: setup, token effects and resolutions are not implemented.
module Arkham.Homebrew.NightOfVespers.Scenarios.TheTeachingsOfAradia (theTeachingsOfAradia) where

import Arkham.Helpers.FlavorText
import Arkham.Homebrew.NightOfVespers.Helpers
import Arkham.Homebrew.NightOfVespers.Intros
import Arkham.Homebrew.NightOfVespers.Jinx (runJinx)
import Arkham.Scenario.Import.Lifted

newtype TheTeachingsOfAradia = TheTeachingsOfAradia ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

theTeachingsOfAradia :: Difficulty -> TheTeachingsOfAradia
theTeachingsOfAradia difficulty = scenario TheTeachingsOfAradia ":night-of-vespers:105" "The Teachings of Aradia" difficulty []

instance RunMessage TheTeachingsOfAradia where
  runMessage msg s@(TheTeachingsOfAradia attrs) =
    runQueueT $ scenarioI18n "theTeachingsOfAradia" $ runJinx msg *> case msg of
      PreScenarioSetup -> do
        runScenarioIntro attrs.id 1
        pure s
      DoStep n PreScenarioSetup -> do
        runScenarioIntro attrs.id n
        pure s
      SearchFound iid ScenarioTarget _ cards -> do
        resolveIntroSearch iid cards
        pure s
      Setup -> do
        flavor $ scope "rules" $ h "title" >> p "body"
        flavor $ scope "scaffold" $ h "title" >> p "body"
        pure s
      _ -> TheTeachingsOfAradia <$> liftRunMessage msg attrs
