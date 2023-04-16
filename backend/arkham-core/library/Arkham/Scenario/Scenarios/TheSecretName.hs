module Arkham.Scenario.Scenarios.TheSecretName
  ( TheSecretName(..)
  , theSecretName
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Difficulty
import Arkham.Helpers.Scenario
import Arkham.Scenario.Runner
import Arkham.Token

newtype TheSecretName = TheSecretName ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSecretName :: Difficulty -> TheSecretName
theSecretName difficulty =
  scenario TheSecretName "05120" "The Secret Name" difficulty []

instance HasTokenValue TheSecretName where
  getTokenValue iid tokenFace (TheSecretName attrs) = case tokenFace of
    Skull -> pure $ toTokenValue attrs Skull 3 5
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ TokenValue Tablet NoModifier
    ElderThing -> pure $ TokenValue ElderThing NoModifier
    otherFace -> getTokenValue iid otherFace attrs

instance RunMessage TheSecretName where
  runMessage msg (TheSecretName attrs) = TheSecretName <$> runMessage msg attrs
