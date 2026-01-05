module Arkham.Scenario.Scenarios.WrittenInRock (writtenInRock) where

import Arkham.EncounterSet qualified as Set
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.WrittenInRock.Helpers

newtype WrittenInRock = WrittenInRock ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

writtenInRock :: Difficulty -> WrittenInRock
writtenInRock difficulty =
  scenario
    WrittenInRock
    "10501"
    "Written in Rock"
    difficulty
    []

instance HasChaosTokenValue WrittenInRock where
  getChaosTokenValue iid tokenFace (WrittenInRock attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage WrittenInRock where
  runMessage msg s@(WrittenInRock attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro1"
      pure s
    Setup -> runScenarioSetup WrittenInRock attrs do
      gather Set.WrittenInRock
    _ -> WrittenInRock <$> liftRunMessage msg attrs
