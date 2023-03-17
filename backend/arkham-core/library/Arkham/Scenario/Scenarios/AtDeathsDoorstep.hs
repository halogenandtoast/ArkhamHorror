module Arkham.Scenario.Scenarios.AtDeathsDoorstep
  ( AtDeathsDoorstep(..)
  , atDeathsDoorstep
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Difficulty
import Arkham.Helpers.Scenario
import Arkham.Scenario.Runner
import Arkham.Token

newtype AtDeathsDoorstep = AtDeathsDoorstep ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

atDeathsDoorstep :: Difficulty -> AtDeathsDoorstep
atDeathsDoorstep difficulty = scenario
  AtDeathsDoorstep
  "05065"
  "At Death's Doorstep"
  difficulty
  []

instance HasTokenValue AtDeathsDoorstep where
  getTokenValue iid tokenFace (AtDeathsDoorstep attrs) = case tokenFace of
    Skull -> pure $ toTokenValue attrs Skull 3 5
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ TokenValue Tablet NoModifier
    ElderThing -> pure $ TokenValue ElderThing NoModifier
    otherFace -> getTokenValue iid otherFace attrs

instance RunMessage AtDeathsDoorstep where
  runMessage msg (AtDeathsDoorstep attrs) =
    AtDeathsDoorstep <$> runMessage msg attrs
