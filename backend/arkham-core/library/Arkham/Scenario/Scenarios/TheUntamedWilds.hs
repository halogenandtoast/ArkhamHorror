module Arkham.Scenario.Scenarios.TheUntamedWilds
  ( TheUntamedWilds(..)
  , theUntamedWilds
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Difficulty
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Message
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheUntamedWilds.Story
import Arkham.Token

newtype TheUntamedWilds = TheUntamedWilds ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theUntamedWilds :: Difficulty -> TheUntamedWilds
theUntamedWilds difficulty = scenario
  TheUntamedWilds
  "04043"
  "The Untamed Wilds"
  difficulty
  [ ".               .            .             .            expeditionCamp .               .              ."
  , ".               pathOfThorns .             .            riverCanyon    .               .              ropeBridge"
  , ".               .            serpentsHaven .            .              circuitousTrail .              ."
  , "templeOfTheFang .            .             ruinsOfEztli .              .               overgrownRuins ."
  ]

instance HasTokenValue TheUntamedWilds where
  getTokenValue iid tokenFace (TheUntamedWilds attrs) = case tokenFace of
    Skull -> pure $ toTokenValue attrs Skull 3 5
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ TokenValue Tablet NoModifier
    ElderThing -> pure $ TokenValue ElderThing NoModifier
    otherFace -> getTokenValue iid otherFace attrs

instance RunMessage TheUntamedWilds where
  runMessage msg s@(TheUntamedWilds attrs) = case msg of
    Setup -> do
      investigatorIds <- getInvestigatorIds
      pushAll $ [story investigatorIds intro]
      pure s
    _ -> TheUntamedWilds <$> runMessage msg attrs
