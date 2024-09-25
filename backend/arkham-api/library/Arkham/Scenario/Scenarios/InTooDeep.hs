module Arkham.Scenario.Scenarios.InTooDeep (InTooDeep (..), inTooDeep) where

import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Helpers.Scenario
import Arkham.Prelude
import Arkham.Scenario.Runner

newtype InTooDeep = InTooDeep ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inTooDeep :: Difficulty -> InTooDeep
inTooDeep difficulty =
  scenario InTooDeep "07123" "In Too Deep" difficulty []

instance HasChaosTokenValue InTooDeep where
  getChaosTokenValue iid tokenFace (InTooDeep attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage InTooDeep where
  runMessage msg (InTooDeep attrs) =
    InTooDeep <$> runMessage msg attrs
