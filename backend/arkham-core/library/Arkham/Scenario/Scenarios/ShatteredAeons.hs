module Arkham.Scenario.Scenarios.ShatteredAeons
  ( ShatteredAeons(..)
  , shatteredAeons
  ) where

import Arkham.Prelude

import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Message
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.ShatteredAeons.Story
import Arkham.Token

newtype ShatteredAeons = ShatteredAeons ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shatteredAeons :: Difficulty -> ShatteredAeons
shatteredAeons difficulty = scenario
  ShatteredAeons
  "04314"
  "Shattered Aeons"
  difficulty
  [ "shoresOfRLyeh   atlantis    ruinsOfNewYork ."
  , "shoresOfRLyeh   atlantis    ruinsOfNewYork valusia"
  , "cityOfTheUnseen nexusOfNKai aPocketInTime  valusia"
  , "cityOfTheUnseen nexusOfNKai aPocketInTime  pnakotus"
  , "yuggoth         mu          plateauOfLeng  pnakotus"
  , "yuggoth         mu          plateauOfLeng  ."
  ]

instance HasTokenValue ShatteredAeons where
  getTokenValue iid tokenFace (ShatteredAeons attrs) = case tokenFace of
    Skull -> pure $ toTokenValue attrs Skull 3 5
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ TokenValue Tablet NoModifier
    ElderThing -> pure $ TokenValue ElderThing NoModifier
    otherFace -> getTokenValue iid otherFace attrs

instance RunMessage ShatteredAeons where
  runMessage msg s@(ShatteredAeons attrs) = case msg of
    Setup -> do
      braziersLit <- getHasRecord TheBraziersAreLit
      foundTheMissingRelic <- getHasRecord TheInvestigatorsFoundTheMissingRelic

      let
        showIntro1 = braziersLit
        showIntro2 = not braziersLit
        showIntro4 = foundTheMissingRelic
        showIntro5 = not foundTheMissingRelic

      investigators <- allInvestigatorIds
      tokens <- getTokensInBag

      let
        cultistCount = count ((== Cultist) . tokenFace) tokens
        tabletCount = count ((== Tablet) . tokenFace) tokens
        additionalSets = case compare cultistCount tabletCount of
          GT -> [EncounterSet.DarkCult]
          LT -> [EncounterSet.AgentsOfYig]
          EQ -> [EncounterSet.DarkCult, EncounterSet.AgentsOfYig]

      encounterDeck <-
        buildEncounterDeck
        $ [ EncounterSet.ShatteredAeons
          , EncounterSet.PnakoticBrotherhood
          , EncounterSet.TemporalFlux
          , EncounterSet.AncientEvils
          ]
        <> additionalSets

      nexusOfNKai <- genCard Locations.nexusOfNKai

      pushAll
        $ [ story investigators intro1 | showIntro1 ]
        <> [ story investigators intro2 | showIntro2 ]
        <> [story investigators intro3]
        <> [ story investigators intro4 | showIntro4 ]
        <> [ story investigators intro5 | showIntro5 ]
        <> [ SetEncounterDeck encounterDeck
           , PlaceLocation nexusOfNKai
           , MoveAllTo (toSource attrs) (toLocationId nexusOfNKai)
           ]

      ShatteredAeons <$> runMessage msg attrs
    _ -> ShatteredAeons <$> runMessage msg attrs
