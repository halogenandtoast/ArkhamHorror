-- Scenario shell only: setup, token effects and resolutions are not implemented.
module Arkham.Homebrew.NightOfVespers.Scenarios.AChorusOfSuffering (aChorusOfSuffering) where

import Arkham.Helpers.FlavorText
import Arkham.Homebrew.NightOfVespers.Helpers
import Arkham.Homebrew.NightOfVespers.Intros
import Arkham.Homebrew.NightOfVespers.Jinx (runJinx)
import Arkham.Scenario.Import.Lifted

newtype AChorusOfSuffering = AChorusOfSuffering ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

aChorusOfSuffering :: Difficulty -> AChorusOfSuffering
aChorusOfSuffering difficulty = scenario AChorusOfSuffering ":night-of-vespers:214" "A Chorus of Suffering" difficulty []

instance RunMessage AChorusOfSuffering where
  runMessage msg s@(AChorusOfSuffering attrs) =
    runQueueT $ scenarioI18n "aChorusOfSuffering" $ runJinx msg *> case msg of
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
      _ -> AChorusOfSuffering <$> liftRunMessage msg attrs
