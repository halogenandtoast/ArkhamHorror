module Arkham.Scenario.Scenarios.BlackStarsRise
  ( BlackStarsRise(..)
  , blackStarsRise
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Helpers.Log
import Arkham.InvestigatorId
import Arkham.Location.Cards qualified as Locations
import Arkham.Message
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.BlackStarsRise.Story
import Arkham.Token

newtype BlackStarsRise = BlackStarsRise ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackStarsRise :: Difficulty -> BlackStarsRise
blackStarsRise difficulty =
  BlackStarsRise
    $ baseAttrs "03274" "Black Stars Rise" difficulty
    & locationLayoutL
    ?~ [ ".           .           northTower      ."
       , "abbeyChurch brokenSteps .               outerWall"
       , ".           .           grandRue        ."
       , ".           .           porteDeLAvancée ."
       ]

instance HasTokenValue BlackStarsRise where
  getTokenValue iid tokenFace (BlackStarsRise attrs) = case tokenFace of
    Skull -> pure $ toTokenValue attrs Skull 3 5
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ TokenValue Tablet NoModifier
    ElderThing -> pure $ TokenValue ElderThing NoModifier
    otherFace -> getTokenValue iid otherFace attrs

data Version = TheFloodBelow | TheVortexAbove

versions :: NonEmpty Version
versions = TheFloodBelow :| [TheVortexAbove]

instance RunMessage BlackStarsRise where
  runMessage msg s@(BlackStarsRise attrs) = case msg of
    Setup -> do
      investigatorIds <- getInvestigatorIds
      ashleighInterviewed <- elem (Recorded $ toCardCode Assets.ashleighClarke)
        <$> getRecordSet VIPsInterviewed

      version <- sample versions

      gatheredCards <- buildEncounterDeck
        [ EncounterSet.BlackStarsRise
        , EncounterSet.EvilPortents
        , EncounterSet.Byakhee
        , EncounterSet.InhabitantsOfCarcosa
        , EncounterSet.TheStranger
        , EncounterSet.DarkCult
        , EncounterSet.AncientEvils
        ]

      let
        tokenToAdd = case scenarioDifficulty attrs of
          Easy -> MinusThree
          Standard -> MinusFive
          Hard -> MinusSix
          Expert -> MinusSeven

      porteDeLAvancee <- genCard Locations.porteDeLAvancee
      grandRue <- genCard Locations.grandRue
      abbeyChurch <- genCard Locations.abbeyChurch
      northTower <- genCard
        =<< sample (Locations.northTower_287 :| [Locations.northTower_288])
      outerWall <- genCard
        =<< sample (Locations.outerWall_285 :| [Locations.outerWall_286])
      brokenSteps <- genCard
        =<< sample (Locations.brokenSteps_289 :| [Locations.brokenSteps_290])

      pushAll
        $ [story investigatorIds intro]
        <> [ story investigatorIds ashleighsInformation | ashleighInterviewed ]
        <> [ AddToken tokenToAdd
           , SetAgendaDeck
           , PlaceLocation porteDeLAvancee
           , PlaceLocation northTower
           , PlaceLocation outerWall
           , PlaceLocation brokenSteps
           , PlaceLocation grandRue
           , PlaceLocation abbeyChurch
           , MoveAllTo (toSource attrs) (toLocationId porteDeLAvancee)
           ]
      BlackStarsRise <$> runMessage
        msg
        (attrs
        & (agendaStackL . at 1 ?~ [Agendas.theTideRises])
        & (agendaStackL . at 2 ?~ [Agendas.theRitualBeginsBlackStarsRise])
        )
    _ -> BlackStarsRise <$> runMessage msg attrs
