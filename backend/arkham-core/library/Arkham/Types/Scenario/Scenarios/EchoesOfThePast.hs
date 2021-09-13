module Arkham.Types.Scenario.Scenarios.EchoesOfThePast
  ( EchoesOfThePast(..)
  , echoesOfThePast
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Acts
import qualified Arkham.Agenda.Cards as Agendas
import qualified Arkham.Asset.Cards as Assets
import qualified Arkham.Enemy.Cards as Enemies
import qualified Arkham.Location.Cards as Locations
import Arkham.Scenarios.EchoesOfThePast.Story
import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Helpers
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import Arkham.Types.Token

newtype EchoesOfThePast = EchoesOfThePast ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

echoesOfThePast :: Difficulty -> EchoesOfThePast
echoesOfThePast difficulty =
  EchoesOfThePast
    $ baseAttrs
        "03120"
        "Echoes of the Past"
        [ Agendas.theTruthIsHidden
        , Agendas.ransackingTheManor
        , Agendas.secretsBetterLeftHidden
        ]
        [Acts.raceForAnswers, Acts.mistakesOfThePast, Acts.theOath]
        difficulty
    & locationLayoutL
    ?~ [ "historicalSociety5 quietHalls1 historicalSociety6"
       , "historicalSociety3 quietHalls2 historicalSociety4"
       , "historicalSociety1 entryHall   historicalSociety2"
       ]

instance HasRecord EchoesOfThePast where
  hasRecord _ = pure False
  hasRecordSet _ = pure []
  hasRecordCount _ = pure 0

instance
  ( Query EnemyMatcher env
  , HasCount DoomCount env EnemyId
  , HasTokenValue env InvestigatorId
  )
  => HasTokenValue env EchoesOfThePast where
  getTokenValue (EchoesOfThePast attrs) iid = \case
    Skull -> do
      enemies <- selectList AnyEnemy
      doomCounts <- traverse (fmap unDoomCount . getCount) enemies
      pure $ toTokenValue
        attrs
        Skull
        (maybe 0 maximum $ fromNullable doomCounts)
        (sum doomCounts)
    Cultist -> pure $ toTokenValue attrs Cultist 2 4
    Tablet -> pure $ toTokenValue attrs Tablet 2 4
    ElderThing -> pure $ toTokenValue attrs ElderThing 2 4
    otherFace -> getTokenValue attrs iid otherFace

gatherTheMidnightMasks :: MonadRandom m => m [EncounterCard]
gatherTheMidnightMasks = traverse
  genEncounterCard
  [ Cards.falseLead
  , Cards.falseLead
  , Cards.huntingShadow
  , Cards.huntingShadow
  , Cards.huntingShadow
  ]

labelLocations :: Text -> [EncounterCard] -> [(EncounterCard, Text)]
labelLocations prefix locations =
  [ (location, prefix <> tshow @Int n) | (location, n) <- zip locations [1 ..] ]

instance ScenarioRunner env => RunMessage env EchoesOfThePast where
  runMessage msg (EchoesOfThePast attrs) = case msg of
    Setup -> do
      partialEncounterDeck <- buildEncounterDeckExcluding
        [Enemies.possessedOathspeaker, Assets.mrPeabody]
        [ EncounterSet.EchoesOfThePast
        , EncounterSet.CultOfTheYellowSign
        , EncounterSet.Delusions
        , EncounterSet.LockedDoors
        , EncounterSet.DarkCult
        ]
      midnightMasks <- gatherTheMidnightMasks
      encounterDeck <- Deck
        <$> shuffleM (unDeck partialEncounterDeck <> midnightMasks)

      groundFloor <- traverse genCard . drop 1 =<< shuffleM
        [ Locations.historicalSocietyMeetingRoom
        , Locations.historicalSocietyRecordOffice_129
        , Locations.historicalSocietyHistoricalMuseum_130
        ]

      secondFloor <- traverse genCard . drop 1 =<< shuffleM
        [ Locations.historicalSocietyHistoricalMuseum_132
        , Locations.historicalSocietyHistoricalLibrary_133
        , Locations.historicalSocietyReadingRoom
        ]

      thirdFloor <- traverse genCard . drop 1 =<< shuffleM
        [ Locations.historicalSocietyHistoricalLibrary_136
        , Locations.historicalSocietyPeabodysOffice
        , Locations.historicalSocietyRecordOffice_138
        ]

      entryHall <- genCard Locations.entryHall
      quietHalls1 <- genCard Locations.quietHalls_131
      quietHalls2 <- genCard Locations.quietHalls_135

      investigatorIds <- getInvestigatorIds

      pushAll
        ([ story investigatorIds intro
         , SetEncounterDeck encounterDeck
         , AddAgenda "03121"
         , AddAct "03124"
         , PlaceLocation entryHall
         , PlaceLocation quietHalls1
         , PlaceLocation quietHalls2
         , MoveAllTo (toSource attrs) (toLocationId entryHall)
         ]
        )

      setAsideCards <- traverse
        genCard
        [ Locations.hiddenLibrary
        , Enemies.possessedOathspeaker
        , Assets.mrPeabody
        , Assets.theTatteredCloak
        , Assets.claspOfBlackOnyx
        ]

      EchoesOfThePast
        <$> runMessage msg (attrs & (setAsideCardsL .~ setAsideCards))
    _ -> EchoesOfThePast <$> runMessage msg attrs
