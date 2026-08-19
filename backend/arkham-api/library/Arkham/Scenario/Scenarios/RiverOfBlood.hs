module Arkham.Scenario.Scenarios.RiverOfBlood (riverOfBlood) where

import Arkham.EncounterSet qualified as Set
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.RiverOfBlood.Helpers

newtype RiverOfBlood = RiverOfBlood ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riverOfBlood :: Difficulty -> RiverOfBlood
riverOfBlood difficulty = scenario RiverOfBlood "13001" "River of Blood" difficulty []

instance HasChaosTokenValue RiverOfBlood where
  getChaosTokenValue iid tokenFace (RiverOfBlood attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage RiverOfBlood where
  runMessage msg s@(RiverOfBlood attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro1"
      pure s
    Setup -> runScenarioSetup RiverOfBlood attrs do
      gather Set.RiverOfBlood
    _ -> RiverOfBlood <$> liftRunMessage msg attrs
