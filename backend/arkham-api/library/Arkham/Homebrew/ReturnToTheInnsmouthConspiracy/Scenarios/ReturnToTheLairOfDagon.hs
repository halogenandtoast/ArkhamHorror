module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Scenarios.ReturnToTheLairOfDagon (returnToTheLairOfDagon) where

import Arkham.Act.CardDefs.TheInnsmouthConspiracy.TheLairOfDagon qualified as Acts
import Arkham.Agenda.CardDefs.TheInnsmouthConspiracy.TheLairOfDagon qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Campaigns.TheInnsmouthConspiracy.Key
import Arkham.Campaigns.TheInnsmouthConspiracy.Memory
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.TheLairOfDagon qualified as Enemies
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.TheVanishingOfElinaHarper qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Acts qualified as HBActs
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Locations qualified as HBLocations
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as HBTreacheries
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers (combineTidalTunnels, scenarioI18n)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Sets qualified as Sets
import Arkham.Investigator.Projection ()
import Arkham.Key
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.TheLairOfDagon qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheInnsmouthConspiracy.TheLairOfDagon
import Arkham.Treachery.CardDefs.TheInnsmouthConspiracy.Syzygy qualified as Treacheries

newtype ReturnToTheLairOfDagon = ReturnToTheLairOfDagon TheLairOfDagon
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToTheLairOfDagon :: Difficulty -> ReturnToTheLairOfDagon
returnToTheLairOfDagon difficulty =
  scenarioWith
    (ReturnToTheLairOfDagon . TheLairOfDagon)
    ":return-to-the-innsmouth-conspiracy:043"
    "Return to TheLairOfDagon"
    difficulty
    []
    (referenceL .~ "07274")

instance RunMessage ReturnToTheLairOfDagon where
  runMessage msg (ReturnToTheLairOfDagon inner@(TheLairOfDagon attrs)) =
    runQueueT $ scenarioI18n "returnToTheLairOfDagon" $ case msg of
      Setup -> runScenarioSetup (ReturnToTheLairOfDagon . TheLairOfDagon) attrs do
        setIsReturnTo
        replaceSet Set.Syzygy Sets.Occultation
        replaceSet Set.LockedDoors Sets.BarricadedDoors
        substitute Acts.theSecondOath HBActs.theSecondOathV2
        encounterWithASecretCult <- hasMemory AnEncounterWithASecretCult
        aDecisionToStickTogether <- hasMemory ADecisionToStickTogether
        _aJailbreak <- hasMemory AJailbreak
        memories <- getRecordSet MemoriesRecovered

        setup $ ul do
          li "gatherSets"
          li "replacedSets"
          li "tidalTunnels"
          li "doorway"
          li "setAsideStirring"
          unscoped $ li "shuffleRemainder"

        gather Sets.ReturnToTheLairOfDagon
        gather Sets.ReturnToFloodedCaverns
        gather Set.TheLairOfDagon
        gather Set.AgentsOfDagon
        gather Set.FloodedCaverns
        gather Set.Syzygy
        gather Set.DarkCult
        gather Set.LockedDoors

        randomizedKeys <- shuffle $ map UnrevealedKey [WhiteKey, YellowKey]
        setAsideKeys $ [BlackKey, BlueKey, GreenKey, PurpleKey, RedKey] <> randomizedKeys

        setAgendaDeck
          [ if encounterWithASecretCult then Agendas.theInitiationV1 else Agendas.theInitiationV2
          , if aDecisionToStickTogether then Agendas.whatLurksBelowV1 else Agendas.whatLurksBelowV2
          , Agendas.theRitualAdvances
          ]

        setActDeck [Acts.theFirstOath, Acts.theSecondOath, Acts.theThirdOath]

        startAt =<< place Locations.grandEntryway
        placeAll [Locations.foulCorridors, Locations.hallOfSilence]
        placeGroup "firstFloorHall" =<< shuffle [Locations.hallOfBlood, Locations.hallOfTheDeep]
        placeGroup "secondFloorHall" =<< shuffle [Locations.hallOfLoyalty, Locations.hallOfRebirth]

        -- "Shuffle the three versions of Doorway to the Depths together, remove two of
        -- them from the game at random without looking. Set the remaining one aside."
        doorway <-
          pickFrom
            ( Locations.doorwayToTheDepths
            , HBLocations.doorwayToTheDepthsV2
            , HBLocations.doorwayToTheDepthsV3
            )
        tunnels <-
          combineTidalTunnels
            <$> amongGathered
              ( CardWithTitle "Tidal Tunnel"
                  <> not_
                    ( mapOneOf
                        cardIs
                        [ Locations.doorwayToTheDepths
                        , HBLocations.doorwayToTheDepthsV2
                        , HBLocations.doorwayToTheDepthsV3
                        ]
                    )
              )
        setAside tunnels
        setAside [doorway, HBTreacheries.stirringInHisSleep]
        setAside
          [ Locations.lairOfDagon
          , Treacheries.syzygy
          , Treacheries.syzygy
          , Treacheries.tidalAlignment
          , Treacheries.tidalAlignment
          , Assets.yhanthleiStatueMysteriousRelic
          , Enemies.apostleOfDagon
          , Enemies.dagonDeepInSlumber
          ]

        case length memories of
          n | n <= 4 -> replicateM_ 5 $ addChaosToken #bless
          n | n >= 5 && n <= 7 -> replicateM_ 2 $ addChaosToken #curse
          _ -> replicateM_ 5 $ addChaosToken #curse

        whenRecoveredMemory AJailbreak do
          mSuspect <- (maybeResult =<<) <$> getCircledRecord PossibleSuspects
          for_ mSuspect \case
            BrianBurnham -> setAside [Enemies.brianBurnhamWantsOut]
            BarnabasMarsh -> setAside [Enemies.barnabasMarshTheChangeIsUponHim]
            OtheraGilman -> setAside [Enemies.otheraGilmanProprietessOfTheHotel]
            ZadokAllen -> setAside [Enemies.zadokAllenDrunkAndDisorderly]
            JoyceLittle -> setAside [Enemies.joyceLittleBookshopOwner]
            RobertFriendly -> setAside [Enemies.robertFriendlyDisgruntledDockworker]

        if aDecisionToStickTogether
          then do
            investigators <- getInvestigators
            thomasDawson <- createAsset =<< genCard Assets.thomasDawsonSoldierInANewWar
            leadChooseOneM do
              questionLabeled "takeControlOfThomasDawson"
              questionLabeledCard Assets.thomasDawsonSoldierInANewWar
              portraits investigators (`takeControlOfAsset` thomasDawson)
          else setAside [Assets.thomasDawsonSoldierInANewWar]
      _ -> ReturnToTheLairOfDagon <$> liftRunMessage msg inner
