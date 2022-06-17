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
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Log
import Arkham.InvestigatorId
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.BlackStarsRise.Story
import Arkham.Target
import Arkham.Token
import Arkham.Trait qualified as Trait

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
  deriving stock Eq

versions :: NonEmpty Version
versions = TheFloodBelow :| [TheVortexAbove]

instance RunMessage BlackStarsRise where
  runMessage msg s@(BlackStarsRise attrs) = case msg of
    Setup -> do
      investigatorIds <- getInvestigatorIds
      ashleighInterviewed <- elem (Recorded $ toCardCode Assets.ashleighClarke)
        <$> getRecordSet VIPsInterviewed

      version <- sample versions

      encounterDeck <- buildEncounterDeckExcluding
        [Enemies.beastOfAldebaran]
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

      let
        (agenda2a, agenda2c) = if version == TheVortexAbove
          then
            ( Agendas.letTheStormRageTheVortexAbove
            , Agendas.theEntityAboveTheVortexAbove
            )
          else
            ( Agendas.letTheStormRageTheFloodBelow
            , Agendas.theEntityAboveTheFloodBelow
            )

      pushAll
        $ [story investigatorIds intro]
        <> [ story investigatorIds ashleighsInformation | ashleighInterviewed ]
        <> [ SearchCollectionForRandom
               iid
               (toSource attrs)
               (CardWithType PlayerTreacheryType <> CardWithOneOf
                 (map
                   CardWithTrait
                   [Trait.Madness, Trait.Pact, Trait.Cultist, Trait.Detective]
                 )
               )
           | iid <- investigatorIds
           ]
        <> [ AddToken tokenToAdd
           , SetAgendaDeck
           , SetEncounterDeck encounterDeck
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
        & (agendaStackL
          . at 1
          ?~ [Agendas.theTideRises, agenda2a, Agendas.theCityFloods]
          )
        & (agendaStackL
          . at 2
          ?~ [ Agendas.theRitualBeginsBlackStarsRise
             , agenda2c
             , Agendas.swallowedSky
             ]
          )
        )
    PlaceDoomOnAgenda -> do
      agendaIds <- selectList AnyAgenda
      leadInvestigatorId <- getLeadInvestigatorId
      push $ chooseOne
        leadInvestigatorId
        [ targetLabel agendaId [PlaceDoom (AgendaTarget agendaId) 1]
        | agendaId <- agendaIds
        ]
      pure s
    _ -> BlackStarsRise <$> runMessage msg attrs
