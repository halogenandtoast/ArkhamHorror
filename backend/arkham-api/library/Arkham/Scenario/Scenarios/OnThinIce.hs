module Arkham.Scenario.Scenarios.OnThinIce (onThinIce) where

import Arkham.EncounterSet qualified as Set
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.OnThinIce.Helpers

newtype OnThinIce = OnThinIce ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onThinIce :: Difficulty -> OnThinIce
onThinIce difficulty =
  scenario
    OnThinIce
    "09609"
    "On Thin Ice"
    difficulty
    []

instance HasChaosTokenValue OnThinIce where
  getChaosTokenValue iid tokenFace (OnThinIce attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage OnThinIce where
  runMessage msg s@(OnThinIce attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro1"
      pure s
    Setup -> runScenarioSetup OnThinIce attrs do
      gather Set.OnThinIce
    _ -> OnThinIce <$> liftRunMessage msg attrs
