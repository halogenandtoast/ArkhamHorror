module Arkham.Scenario.Scenarios.TheCityOfArchives (theCityOfArchives, TheCityOfArchives (..), setupTheCityOfArchives) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignStep
import Arkham.Campaigns.TheForgottenAge.Key
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.Scenario
import Arkham.Helpers.Xp
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (enemyAt)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted hiding (getIsReturnTo)
import Arkham.Scenario.Types (setAsideCardsL)
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheCityOfArchives.Helpers
import Arkham.Timing qualified as Timing
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

setupTheCityOfArchives :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> ScenarioBuilderT m ()
setupTheCityOfArchives _attrs = do
  cooperated <- getHasRecord TheInvestigatorsCooperatedWithTheYithians
  setup do
    ul do
      li "gatherSets"
      li "beforeDrawingOpeningHandsItems"
      li "beforeDrawingOpeningHandsAlejandro"
      li "bodyOfAYithian"
      li.nested "checkCampaignLog" do
        li.validate cooperated "cooperated1"
        li.validate (not cooperated) "resisted1"
      li.nested "checkCampaignLog" do
        li.validate cooperated "cooperated2"
        li.validate (not cooperated) "resisted2"
      li "placeLocations"
      li "setAside"
      unscoped $ li "shuffleRemainder"

  scope "bodyOfAYithian" $ flavor $ h "title" >> p "body"

  whenReturnTo $ gather Set.ReturnToTheCityOfArchives
  gather Set.TheCityOfArchives
  gather Set.AgentsOfYogSothoth
  gather Set.LockedDoors
  gather Set.ChillingCold
  gather Set.StrikingFear

  isReturnTo <- getIsReturnTo
  setActDeck
    [ Acts.exploringPnakotus
    , if isReturnTo then Acts.unrestrictedAccess else Acts.restrictedAccess
    , Acts.repossession
    ]
  setAgendaDeck [Agendas.cityOfTheGreatRace, Agendas.lostMemories, Agendas.humanityFading]

  if cooperated
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
      , if cooperated
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

  whenReturnTo $ place_ Locations.hallsOfPnakotusSouthernCorridors

  setAside
    [ Locations.greatLibrary
    , Locations.yithianOrrery
    , Locations.laboratoryOfTheGreatRace
    , Locations.deconstructionRoom
    , Locations.towersOfPnakotus
    , Assets.theCustodian
    ]

  whenReturnTo $ setAside [Locations.cyclopeanVaults, Locations.alienConservatory]

instance RunMessage TheCityOfArchives where
  runMessage msg s@(TheCityOfArchives attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      setChaosTokens standaloneChaosTokens
      pure s
    Do (CheckWindows [Window Timing.When (Window.DrawingStartingHand iid) _]) -> do
      uniqueItemAssetCards <- select $ inDeckOf iid <> basic (#asset <> #item <> CardIsUnique)
      uniqueItemAssets <- select $ #item <> UniqueAsset
      mAlejandro <- selectOne $ inDeckOf iid <> basic (cardIs Assets.alejandroVela)

      let setAsideUpdate = maybe id (over setAsideCardsL . (:)) mAlejandro

      pushAll
        $ map (RemovePlayerCardFromGame True) uniqueItemAssetCards
        <> [ RemovePlayerCardFromGame True alejandro
           | alejandro <- maybeToList mAlejandro
           ]
        <> map (RemoveFromGame . AssetTarget) uniqueItemAssets
      pure . TheCityOfArchives $ attrs & setAsideUpdate
    PreScenarioSetup -> scope "intro" do
      eachInvestigator $ push . BecomeYithian

      storyWithChooseOneM' (h "title" >> p "intro1") do
        labeled' "cooperate" do
          flavor $ h "title" >> p "intro2"
          record TheInvestigatorsCooperatedWithTheYithians
        labeled' "resist" do
          flavor $ h "title" >> p "intro3"
          record TheInvestigatorsResistedCaptivity
      pure s
    Setup -> runScenarioSetup TheCityOfArchives attrs $ setupTheCityOfArchives attrs
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
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolution "noResolution"
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
              , ReadAboutEarth
              , SawAFamiliarSpecimen
              ]
          resignedWithTheCustodian <-
            orM
              [ resignedWith Assets.theCustodian
              , selectAny (AssetControlledBy Anyone <> assetIs Assets.theCustodian)
              ]

          let
            totalTasks = rememberedTasks + if resignedWithTheCustodian then 1 else 0
            (logEntry, bonusXp) = case totalTasks of
              n | n >= 6 -> (TheProcessWasPerfected, toBonus "perfected" 4)
              n | n == 5 -> (TheProcessWasSuccessful, toBonus "successful" 2)
              n | n == 4 -> (TheProcessBackfired, toBonus "backfired" 1)
              n | n == 3 -> (TheProcessBackfiredSpectacularly, NoBonus)
              _ -> error "Invalid number of tasks"
          xp <- allGainXpWithBonus' attrs bonusXp
          resolutionFlavor $ scope "resolution1" do
            setTitle "title"
            p "body"
            ul do
              li.nested "tasks" do
                li.validate (totalTasks >= 6) "tasks6"
                li.validate (totalTasks == 5) "tasks5"
                li.validate (totalTasks == 4) "tasks4"
                li.validate (totalTasks == 3) "tasks3"
              withVars ["xp" .= xp] $ li "xp"

          record logEntry
          endOfScenarioThen
            $ UpgradeDeckStep
            $ InterludeStep 4 (guard resignedWithTheCustodian $> TheCustodianWasUnderControl)
        _ -> error "Invalid resolution"
      pure s
    _ -> TheCityOfArchives <$> liftRunMessage msg attrs
