module Arkham.Scenario.Scenarios.CongressOfTheKeys (congressOfTheKeys) where

import Arkham.EncounterSet qualified as Set
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.CongressOfTheKeys.Helpers

newtype CongressOfTheKeys = CongressOfTheKeys ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

congressOfTheKeys :: Difficulty -> CongressOfTheKeys
congressOfTheKeys difficulty =
  scenario
    CongressOfTheKeys
    "09694"
    "Congress of the Keys"
    difficulty
    []

instance HasChaosTokenValue CongressOfTheKeys where
  getChaosTokenValue iid tokenFace (CongressOfTheKeys attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage CongressOfTheKeys where
  runMessage msg s@(CongressOfTheKeys attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro1"
      pure s
    Setup -> runScenarioSetup CongressOfTheKeys attrs do
      gather Set.CongressOfTheKeys
    _ -> CongressOfTheKeys <$> liftRunMessage msg attrs
