module Arkham.Scenario.Scenarios.InTheClutchesOfChaos (
  InTheClutchesOfChaos (..),
  inTheClutchesOfChaos,
) where

import Arkham.Prelude

import Arkham.CampaignLogKey
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.InTheClutchesOfChaos.Story

newtype InTheClutchesOfChaos = InTheClutchesOfChaos ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inTheClutchesOfChaos :: Difficulty -> InTheClutchesOfChaos
inTheClutchesOfChaos difficulty =
  scenario
    InTheClutchesOfChaos
    "05284"
    "In the Clutches of Chaos"
    difficulty
    []

instance HasChaosTokenValue InTheClutchesOfChaos where
  getChaosTokenValue iid tokenFace (InTheClutchesOfChaos attrs) = case tokenFace of
    Skull -> do
      doom <- getSum <$> selectAgg Sum LocationDoom (locationWithInvestigator iid)
      breaches <- getSum <$> selectAgg Sum LocationBreaches (locationWithInvestigator iid)
      pure $ toChaosTokenValue attrs Skull (doom + breaches) (doom + breaches + 1)
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage InTheClutchesOfChaos where
  runMessage msg s@(InTheClutchesOfChaos attrs) = case msg of
    PreScenarioSetup -> do
      investigators <- allInvestigators
      neverSeenOrHeardFromAgain <- getHasRecord TheInvestigatorsAreNeverSeenOrHeardFromAgain
      pushAll $
        [story investigators intro1]
          <> [story investigators intro2 | neverSeenOrHeardFromAgain]
          <> [story investigators intro3 | not neverSeenOrHeardFromAgain]
          <> [story investigators intro4]
      pure s
    Setup -> do
      encounterDeck <-
        buildEncounterDeck
          [ EncounterSet.InTheClutchesOfChaos
          , EncounterSet.AgentsOfAzathoth
          , EncounterSet.Nightgaunts
          ]
      frenchHill <- sample $ Locations.frenchHill_290 :| [Locations.frenchHill_291]
      rivertown <- sample $ Locations.rivertown_292 :| [Locations.rivertown_293]
      southside <- sample $ Locations.southside_294 :| [Locations.southside_295]
      uptown <- sample $ Locations.uptown_296 :| [Locations.uptown_297]
      southChurch <- sample $ Locations.southChurch_298 :| [Locations.southChurch_299]
      merchantDistrict <- sample $ Locations.merchantDistrict_300 :| [Locations.merchantDistrict_301]

      (southsideId, placeSouthside) <- placeLocationCard southside
      placeRest <- placeLocationCards_ [frenchHill, rivertown, uptown, southChurch, merchantDistrict]

      pushAll $
        [ SetEncounterDeck encounterDeck
        , SetAgendaDeck
        , SetActDeck
        , placeSouthside
        , MoveAllTo (toSource attrs) southsideId
        ]
          <> placeRest
      pure s
    _ -> InTheClutchesOfChaos <$> runMessage msg attrs
