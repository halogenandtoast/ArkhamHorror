module Arkham.Scenario.Scenarios.TheDoomOfEztli
  ( TheDoomOfEztli(..)
  , theDoomOfEztli
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Difficulty
import Arkham.InvestigatorId
import Arkham.Scenario.Runner
import Arkham.Token

newtype TheDoomOfEztli = TheDoomOfEztli ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDoomOfEztli :: Difficulty -> TheDoomOfEztli
theDoomOfEztli difficulty = TheDoomOfEztli $ baseAttrs
  "TODO: ID"
  "TODO: NAME"
  difficulty

instance HasTokenValue TheDoomOfEztli where
  getTokenValue iid tokenFace (TheDoomOfEztli attrs) = case tokenFace of
    Skull -> pure $ toTokenValue attrs Skull 3 5
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ TokenValue Tablet NoModifier
    ElderThing -> pure $ TokenValue ElderThing NoModifier
    otherFace -> getTokenValue iid otherFace attrs

instance RunMessage TheDoomOfEztli where
  runMessage msg (TheDoomOfEztli attrs) =
    TheDoomOfEztli <$> runMessage msg attrs
