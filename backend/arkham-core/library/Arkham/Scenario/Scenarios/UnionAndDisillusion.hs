module Arkham.Scenario.Scenarios.UnionAndDisillusion (
  UnionAndDisillusion (..),
  unionAndDisillusion,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Difficulty
import Arkham.Helpers.Scenario
import Arkham.Scenario.Runner
import Arkham.Token

newtype UnionAndDisillusion = UnionAndDisillusion ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unionAndDisillusion :: Difficulty -> UnionAndDisillusion
unionAndDisillusion difficulty =
  scenario
    UnionAndDisillusion
    "05238"
    "Union and Disillusion"
    difficulty
    []

instance HasTokenValue UnionAndDisillusion where
  getTokenValue iid tokenFace (UnionAndDisillusion attrs) = case tokenFace of
    Skull -> pure $ toTokenValue attrs Skull 3 5
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ TokenValue Tablet NoModifier
    ElderThing -> pure $ TokenValue ElderThing NoModifier
    otherFace -> getTokenValue iid otherFace attrs

instance RunMessage UnionAndDisillusion where
  runMessage msg (UnionAndDisillusion attrs) =
    UnionAndDisillusion <$> runMessage msg attrs
