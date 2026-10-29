-- Scenario shell only: setup, token effects and resolutions are not implemented.
module Arkham.Homebrew.NightOfVespers.Scenarios.TheEndOfSeasons (theEndOfSeasons) where

import Arkham.Helpers.FlavorText
import Arkham.Homebrew.NightOfVespers.Helpers
import Arkham.Homebrew.NightOfVespers.Intros
import Arkham.Homebrew.NightOfVespers.Jinx (runJinx)
import Arkham.Scenario.Import.Lifted

newtype TheEndOfSeasons = TheEndOfSeasons ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

theEndOfSeasons :: Difficulty -> TheEndOfSeasons
theEndOfSeasons difficulty = scenario TheEndOfSeasons ":night-of-vespers:001" "The End of Seasons" difficulty []

instance RunMessage TheEndOfSeasons where
  runMessage msg s@(TheEndOfSeasons attrs) =
    runQueueT $ scenarioI18n "theEndOfSeasons" $ runJinx msg *> case msg of
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
      _ -> TheEndOfSeasons <$> liftRunMessage msg attrs
