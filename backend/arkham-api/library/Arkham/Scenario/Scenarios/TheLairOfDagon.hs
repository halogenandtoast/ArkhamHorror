module Arkham.Scenario.Scenarios.TheLairOfDagon (TheLairOfDagon (..), theLairOfDagon) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Campaigns.TheInnsmouthConspiracy.Memory
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Log
import Arkham.Key
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheLairOfDagon.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheLairOfDagon = TheLairOfDagon ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theLairOfDagon :: Difficulty -> TheLairOfDagon
theLairOfDagon difficulty =
  scenario
    TheLairOfDagon
    "07274"
    "The Lair of Dagon"
    difficulty
    [ ". thirdFloorHall ."
    , "secondFloorHall1 foulCorridors secondFloorHall2"
    , "firstFloorHall1 grandEntryway firstFloorHall2"
    ]

instance HasChaosTokenValue TheLairOfDagon where
  getChaosTokenValue iid tokenFace (TheLairOfDagon attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheLairOfDagon where
  runMessage msg s@(TheLairOfDagon attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro1"
      missionWasSuccessful <- getHasRecord TheMissionWasSuccessful
      if missionWasSuccessful
        then doStep 2 PreScenarioSetup
        else doStep 3 PreScenarioSetup
      selectForMaybeM (InDeckOf Anyone <> basic (cardIs Assets.elinaHarperKnowsTooMuch)) obtainCard
      pure s
    DoStep 2 PreScenarioSetup -> do
      story $ i18nWithTitle "intro2"
      stuckTogether <- hasMemory ADecisionToStickTogether
      if stuckTogether
        then doStep 4 PreScenarioSetup
        else doStep 5 PreScenarioSetup
      pure s
    DoStep 3 PreScenarioSetup -> do
      story $ i18nWithTitle "intro3"
      stuckTogether <- hasMemory ADecisionToStickTogether
      if stuckTogether
        then doStep 4 PreScenarioSetup
        else doStep 5 PreScenarioSetup
      pure s
    DoStep 4 PreScenarioSetup -> do
      story $ i18nWithTitle "intro4"
      pure s
    DoStep 5 PreScenarioSetup -> do
      story $ i18nWithTitle "intro5"
      pure s
    Setup -> runScenarioSetup TheLairOfDagon attrs do
      gather Set.TheLairOfDagon
      gather Set.AgentsOfDagon
      gather Set.FloodedCaverns
      gather Set.Syzygy
      gather Set.DarkCult
      gather Set.LockedDoors

      randomizedKeys <- shuffle $ map UnrevealedKey [WhiteKey, YellowKey]
      setAsideKeys $ [BlackKey, BlueKey, GreenKey, PurpleKey, RedKey] <> randomizedKeys

      encounterWithASecretCult <- hasMemory AnEncounterWithASecretCult
      aDecisionToStickTogether <- hasMemory ADecisionToStickTogether
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

      setAside =<< amongGathered (CardWithTitle "Tidal Tunnel")
      setAside
        [ Locations.lairOfDagon
        , Treacheries.syzygy
        , Treacheries.syzygy
        , Treacheries.tidalAlignment
        , Treacheries.tidalAlignment
        , Assets.yhanthleiStatusMysteriousRelic
        , Enemies.apostleOfDagon
        , Enemies.dagonDeepInSlumber
        ]

      memories <- getRecordSet MemoriesRecovered

      case length memories of
        n | n <= 4 -> replicateM_ 5 $ addChaosToken #bless
        n | n >= 5 && n <= 7 -> replicateM_ 2 $ addChaosToken #curse
        _ -> replicateM_ 5 $ addChaosToken #curse

      whenRecoveredMemory AJailbreak do
        mSuspect <- maybeResult <$$> getCircledRecord PossibleSuspects
        for_ (join mSuspect) \case
          BrianBurnham -> setAside [Enemies.brianBurnhamWantsOut]
          BarnabasMarsh -> setAside [Enemies.barnabasMarshTheChangeIsUponHim]
          OtheraGilman -> setAside [Enemies.otheraGilmanProprietessOfTheHotel]
          ZadokAllen -> setAside [Enemies.zadokAllenDrunkAndDisorderly]
          JoyceLittle -> setAside [Enemies.joyceLittleBookshopOwner]
          RobertFriendly -> setAside [Enemies.robertFriendlyDisgruntledDockworker]
        pure ()
    _ -> TheLairOfDagon <$> liftRunMessage msg attrs
