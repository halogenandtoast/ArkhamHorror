module Arkham.Scenario.Scenarios.TheCityOfArchives (TheCityOfArchives (..), theCityOfArchives) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (enemyAt)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (setAsideCardsL)
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheCityOfArchives.Helpers
import Arkham.Scenarios.TheCityOfArchives.Story
import Arkham.Timing qualified as Timing
import Arkham.Trait hiding (Trait (Cultist, ElderThing))
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window
import Control.Lens (over)

newtype TheCityOfArchives = TheCityOfArchives ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCityOfArchives :: Difficulty -> TheCityOfArchives
theCityOfArchives difficulty =
  scenario
    TheCityOfArchives
    "04237"
    "The City of Archives"
    difficulty
    [ ".                yithianOrrery                   laboratoryOfTheGreatRace         deconstructionRoom              ."
    , ".                .                               hallsOfPnakotusNorthernCorridors .                               interviewRoom1"
    , "towersOfPnakotus hallsOfPnakotusWesternCorridors .                                hallsOfPnakotusEasternCorridors interviewRoom2"
    , ".                greatLibrary                    .                                .                               interviewRoom3"
    ]

instance HasChaosTokenValue TheCityOfArchives where
  getChaosTokenValue iid chaosTokenFace (TheCityOfArchives attrs) = case chaosTokenFace of
    Skull -> do
      cardsInHand <- fieldMap InvestigatorHand length iid
      pure
        $ if cardsInHand >= 5
          then
            ChaosTokenValue Skull
              $ if isEasyStandard attrs
                then NegativeModifier 3
                else AutoFailModifier
          else toChaosTokenValue attrs Skull 1 2
    Cultist -> pure $ ChaosTokenValue Cultist $ NegativeModifier 2
    Tablet -> pure $ ChaosTokenValue Tablet $ NegativeModifier 3
    ElderThing -> pure $ ChaosTokenValue ElderThing $ NegativeModifier 2
    otherFace -> getChaosTokenValue iid otherFace attrs

standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ PlusOne
  , Zero
  , Zero
  , Zero
  , MinusOne
  , MinusTwo
  , MinusTwo
  , MinusThree
  , MinusFive
  , Skull
  , Skull
  , Cultist
  , ElderThing
  , AutoFail
  , ElderSign
  ]

instance RunMessage TheCityOfArchives where
  runMessage msg s@(TheCityOfArchives attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      setChaosTokens standaloneChaosTokens
      pure s
    Do (CheckWindows [Window Timing.When (Window.DrawingStartingHand iid) _]) -> do
      uniqueItemAssetCards <- select $ InDeckOf (InvestigatorWithId iid) <> basic (#item <> CardIsUnique)
      uniqueItemAssets <- select $ AssetWithTrait Item <> UniqueAsset

      mAlejandro <-
        selectOne
          $ InDeckOf (InvestigatorWithId iid)
          <> BasicCardMatch
            (cardIs Assets.alejandroVela)

      let setAsideUpdate = maybe id (over setAsideCardsL . (:)) mAlejandro

      pushAll
        $ map (RemovePlayerCardFromGame True) uniqueItemAssetCards
        <> [ RemovePlayerCardFromGame True alejandro
           | alejandro <- maybeToList mAlejandro
           ]
        <> map (RemoveFromGame . AssetTarget) uniqueItemAssets
      pure . TheCityOfArchives $ attrs & setAsideUpdate
    PreScenarioSetup -> do
      eachInvestigator $ push . BecomeYithian

      story intro1
      lead <- getLead
      chooseOneM lead do
        labeled "Cooperate and tell the creatures everything you know." do
          story intro2
          record TheInvestigatorsCooperatedWithTheYithians
        labeled "Refuse and resist captivity." do
          story intro3
          record TheInvestigatorsResistedCaptivity
      pure s
    Setup -> runScenarioSetup TheCityOfArchives attrs do
      gather Set.TheCityOfArchives
      gather Set.AgentsOfYogSothoth
      gather Set.LockedDoors
      gather Set.ChillingCold
      gather Set.StrikingFear

      setActDeck [Acts.exploringPnakotus, Acts.restrictedAccess, Acts.repossession]
      setAgendaDeck [Agendas.cityOfTheGreatRace, Agendas.lostMemories, Agendas.humanityFading]

      cooperatedWithTheYithians <- getHasRecord TheInvestigatorsCooperatedWithTheYithians
      if cooperatedWithTheYithians
        then do
          interviewRoom <- placeLabeled "interviewRoom1" Locations.interviewRoomArrivalChamber
          startAt interviewRoom
          enemyAt_ Enemies.yithianObserver interviewRoom
        else do
          startAt =<< placeLabeled "interviewRoom1" Locations.interviewRoomRestrainingChamber
          placeInVictory [Enemies.yithianObserver]

      otherRooms <-
        shuffleM
          [ Locations.interviewRoomIchorFilledChamber
          , if cooperatedWithTheYithians
              then Locations.interviewRoomRestrainingChamber
              else Locations.interviewRoomArrivalChamber
          ]

      for_ (zip [2 ..] otherRooms) $ \(idx, location) -> do
        placeLabeled_ ("interviewRoom" <> tshow @Int idx) location

      placeAll
        [ Locations.hallsOfPnakotusNorthernCorridors
        , Locations.hallsOfPnakotusEasternCorridors
        , Locations.hallsOfPnakotusWesternCorridors
        ]

      setAside
        [ Locations.greatLibrary
        , Locations.yithianOrrery
        , Locations.laboratoryOfTheGreatRace
        , Locations.deconstructionRoom
        , Locations.towersOfPnakotus
        , Assets.theCustodian
        ]
    ResolveChaosToken _ chaosTokenFace iid | isHardExpert attrs && chaosTokenFace `elem` [Cultist, ElderThing] -> do
      push $ InvestigatorPlaceCluesOnLocation iid (ChaosTokenEffectSource chaosTokenFace) 1
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> do
      case token.face of
        face | face `elem` [Cultist, ElderThing] -> do
          push $ InvestigatorPlaceCluesOnLocation iid (ChaosTokenEffectSource face) 1
        Tablet -> randomDiscardN iid Tablet $ if isEasyStandard attrs then 1 else n
        _ -> pure ()
      pure s
    ScenarioResolution r -> do
      case r of
        NoResolution -> do
          story noResolution
          record TheInvestigatorsHadTheirMemoriesExpunged
          eachInvestigator drivenInsane
          gameOver
        Resolution 1 -> do
          rememberedTasks <-
            countM
              remembered
              [ FoundTheProcess
              , DissectedAnOrgan
              , InterviewedASubject
              , RealizedWhatYearItIs
              , ActivatedTheDevice
              ]
          resignedWithTheCustodian <-
            orM
              [ resignedWith Assets.theCustodian
              , selectAny (AssetControlledBy Anyone <> assetIs Assets.theCustodian)
              ]

          let
            totalTasks = rememberedTasks + if resignedWithTheCustodian then 1 else 0
            (logEntry, bonusXp) = case totalTasks of
              n | n == 6 -> (TheProcessWasPerfected, toBonus "perfected" 4)
              n | n == 5 -> (TheProcessWasSuccessful, toBonus "successful" 2)
              n | n == 4 -> (TheProcessBackfired, toBonus "backfired" 1)
              n | n == 3 -> (TheProcessBackfiredSpectacularly, NoBonus)
              _ -> error "Invalid number of tasks"

          story resolution1
          record logEntry
          allGainXpWithBonus attrs bonusXp
          endOfScenarioThen
            $ UpgradeDeckStep
            $ InterludeStep 4 (guard resignedWithTheCustodian $> TheCustodianWasUnderControl)
        _ -> error "Invalid resolution"
      pure s
    _ -> TheCityOfArchives <$> liftRunMessage msg attrs
