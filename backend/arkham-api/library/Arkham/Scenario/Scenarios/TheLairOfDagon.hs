module Arkham.Scenario.Scenarios.TheLairOfDagon (TheLairOfDagon (..), theLairOfDagon) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Agenda.Sequence
import Arkham.Agenda.Types (Field (..))
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Campaigns.TheInnsmouthConspiracy.Memory
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.I18n
import Arkham.Investigator.Projection ()
import Arkham.Key
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (Field (ScenarioKeys))
import Arkham.Scenarios.TheLairOfDagon.Helpers
import Arkham.Trait (Trait (Suspect))
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
    Skull -> do
      n <- length <$> scenarioField ScenarioKeys
      pure $ toChaosTokenValue attrs Skull n (n * 2)
    Cultist -> pure $ toChaosTokenValue attrs Skull 0 (-2)
    Tablet -> pure $ ChaosTokenValue Tablet (NegativeModifier 3)
    ElderThing -> pure $ ChaosTokenValue ElderThing (NegativeModifier 4)
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
    StandaloneSetup -> do
      {- FOURMOLU_DISABLE -}
      setChaosTokens
        [ #"+1" , #"0" , #"0" , #"-1" , #"-1" , #"-2" , #"-2" , #"-3" , #"-4"
        , Skull , Skull , Cultist , Cultist , Tablet , Tablet , ElderThing , ElderThing
        , AutoFail , ElderSign
        ]
      {- FOURMOLU_ENABLE -}
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
        , Assets.yhanthleiStatueMysteriousRelic
        , Enemies.apostleOfDagon
        , Enemies.dagonDeepInSlumber
        ]

      memories <- getRecordSet MemoriesRecovered

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
          lead <- getLead
          investigators <- getInvestigators
          thomasDawson <- createAsset =<< genCard Assets.thomasDawsonSoldierInANewWar
          chooseTargetM lead investigators (`takeControlOfAsset` thomasDawson)
        else setAside [Assets.thomasDawsonSoldierInANewWar]
    ResolveChaosToken _ Cultist iid -> do
      drawAnotherChaosToken iid
      withSkillTest \sid -> onRevealChaosTokenEffect sid #curse Cultist sid failSkillTest
      pure s
    ResolveChaosToken _ Tablet iid -> do
      when (isHardExpert attrs) do
        withLocationOf iid \lid -> do
          ks <- iid.keys
          for_ ks $ placeKey lid
          assignDamage iid Tablet 1
      pure s
    ResolveChaosToken _ ElderThing _iid -> do
      when (isHardExpert attrs) do
        n <- min 2 <$> getRemainingCurseTokens
        replicateM_ n $ addChaosToken #curse
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _n -> do
      case token.face of
        Tablet | isEasyStandard attrs -> do
          withLocationOf iid \lid -> do
            ks <- iid.keys
            for_ ks $ placeKey lid
        ElderThing | isEasyStandard attrs -> do
          n <- getRemainingCurseTokens
          when (n > 0) $ addChaosToken #curse
        _ -> pure ()
      pure s
    HandleNoRemainingInvestigators (isTarget attrs -> True) -> do
      selectOne AnyAgenda >>= \case
        Nothing -> TheLairOfDagon <$> liftRunMessage msg attrs
        Just aid ->
          field AgendaSequence aid >>= \case
            Sequence 3 B -> pure s
            _ -> TheLairOfDagon <$> liftRunMessage msg attrs
    ScenarioResolution resolution -> scope "resolutions" do
      case resolution of
        NoResolution -> do
          story $ i18nWithTitle "noResolution"
          record DagonHasAwakened
          push R1
        Resolution 1 -> do
          allGainXp attrs

          gateKeeperDefeated <- selectAny (VictoryDisplayCardMatch $ basic $ CardWithTrait Suspect)
          recordWhen gateKeeperDefeated TheGatekeeperHasBeenDefeated
          storyWithChooseOne
            (i18nWithTitle "resolution1")
            [ Label "Tell Oceiros nothing" [R2]
            , Label "Lie to Oceiros" [R3]
            , Label "Tell Oceiros everything" [R4]
            ]
        Resolution 2 -> do
          story $ i18nWithTitle "resolution2"
          addChaosToken #elderthing
          endOfScenario
        Resolution 3 -> do
          story $ i18nWithTitle "resolution3"
          addChaosToken #tablet
          endOfScenario
        Resolution 4 -> do
          story $ i18nWithTitle "resolution4"
          addChaosToken #cultist
          endOfScenario
        _ -> error "Invalid resolution"
      pure s
    _ -> TheLairOfDagon <$> liftRunMessage msg attrs
