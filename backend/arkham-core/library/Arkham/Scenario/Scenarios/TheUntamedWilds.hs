module Arkham.Scenario.Scenarios.TheUntamedWilds
  ( TheUntamedWilds(..)
  , theUntamedWilds
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Message
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheUntamedWilds.Story
import Arkham.Treachery.Cards qualified as Treacheries
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
  runMessage msg (TheUntamedWilds attrs) = case msg of
    Setup -> do
      investigatorIds <- getInvestigatorIds
      expeditionCamp <- genCard Locations.expeditionCamp

      explorationDeck <- shuffleM =<< traverse
        genCard
        [ Locations.pathOfThorns
        , Locations.riverCanyon
        , Locations.ropeBridge
        , Locations.serpentsHaven
        , Locations.circuitousTrail
        , Treacheries.lostInTheWilds
        , Treacheries.overgrowth
        , Treacheries.snakeBite
        , Treacheries.lowOnSupplies
        , Treacheries.arrowsFromTheTrees
        ]
      pushAll
        $ [ story investigatorIds intro
          , SetAgendaDeck
          , SetActDeck
          , PlaceLocation expeditionCamp
          , MoveAllTo (toSource attrs) (toLocationId expeditionCamp)
          ]
      TheUntamedWilds <$> runMessage
        msg
        (attrs
        & (decksL . at ExplorationDeck ?~ explorationDeck)
        & (actStackL
          . at 1
          ?~ [ Acts.exploringTheRainforest
             , Acts.huntressOfTheEztli
             , Acts.searchForTheRuins
             , Acts.theGuardedRuins
             ]
          )
        & (agendaStackL
          . at 1
          ?~ [Agendas.expeditionIntoTheWild, Agendas.intruders]
          )
        )
    _ -> TheUntamedWilds <$> runMessage msg attrs
