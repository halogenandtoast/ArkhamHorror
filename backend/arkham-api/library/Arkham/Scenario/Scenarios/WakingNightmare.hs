module Arkham.Scenario.Scenarios.WakingNightmare (WakingNightmare (..), wakingNightmare) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheDreamEaters.ChaosBag
import Arkham.Campaigns.TheDreamEaters.Helpers
import Arkham.Campaigns.TheDreamEaters.Meta
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Agenda
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Resolution
import Arkham.Scenario.Helpers hiding (addCampaignCardToDeckChoice)
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.WakingNightmare.FlavorText
import Arkham.Scenarios.WakingNightmare.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Trait (Trait (Staff))
import Arkham.Treachery.Cards qualified as Treacheries

newtype WakingNightmare = WakingNightmare ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wakingNightmare :: Difficulty -> WakingNightmare
wakingNightmare difficulty =
  scenario
    WakingNightmare
    "06063"
    "Waking Nightmare"
    difficulty
    [ ".             recordsOffice .                         ."
    , ".             waitingRoom   .                         ."
    , "emergencyRoom .             experimentalTherapiesWard ."
    , ".             basementDoor1 stairwell                 basementDoor3"
    , ".             .             basementDoor2             ."
    ]

instance HasChaosTokenValue WakingNightmare where
  getChaosTokenValue iid tokenFace (WakingNightmare attrs) = case tokenFace of
    Skull -> do
      isEngaged <- selectAny $ EnemyWithTrait Staff <> enemyEngagedWith iid
      pure
        $ if isEngaged
          then toChaosTokenValue attrs Skull 3 5
          else toChaosTokenValue attrs Skull 1 3
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    ElderThing -> do
      n <- selectCount InfestedLocation
      pure $ toChaosTokenValue attrs ElderThing n (n + 1)
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage WakingNightmare where
  runMessage msg s@(WakingNightmare attrs) = runQueueT $ case msg of
    StandaloneSetup -> do
      setChaosTokens $ initChaosBag TheWebOfDreams attrs.difficulty
      pure s
    PreScenarioSetup -> do
      storyWithChooseOneM intro1 do
        labeled
          "Convince Doctor Maheswaran to come with you while you investigate, for her safety and yours."
          do
            story intro2
            record DrMaheswaranJoinedTheInvestigation
        labeled
          "Convince Doctor Maheswaran to stay with the patients and keep them safe while you investigate."
          do
            story intro3
            record DrMaheswaranStayedWithHerPatients
      pure s
    Setup -> runScenarioSetup WakingNightmare attrs do
      gather Set.WakingNightmare
      gather Set.MergingRealities
      gather Set.WhispersOfHypnos
      gather Set.LockedDoors
      gather Set.StrikingFear

      drMaheswaranInPlay <- getHasRecord DrMaheswaranJoinedTheInvestigation

      startAt =<< place Locations.waitingRoom
      placeAll [Locations.emergencyRoom, Locations.experimentalTherapiesWard, Locations.recordsOffice]

      when drMaheswaranInPlay do
        lead <- getLead
        beginWithStoryAsset lead Assets.drShivaniMaheswaran

      setAgendaDeck [Agendas.hallsOfStMarys, Agendas.theInfestationSpreads, Agendas.hospitalOfHorrors]
      setActDeck [Acts.lookingForAnswers, Acts.searchForThePatient, Acts.containingTheOutbreak]
      setAside =<< amongGathered (mapOneOf cardIs [Enemies.corruptedOrderly, Treacheries.outbreak])
      setAside
        $ [ Locations.stairwell
          , Locations.morgue
          , Locations.operatingRoom
          , Locations.privateRoom
          , Assets.randolphCarterChainedToTheWakingWorld
          , Stories.theInfestationBegins
          ]
        <> (guard (not drMaheswaranInPlay) *> [Assets.drShivaniMaheswaran])
    ResolveChaosToken _ Cultist iid -> do
      n <- getCurrentAgendaStep
      when (isHardExpert attrs && n >= 2) makeInfestationTest
      drawAnotherChaosToken iid
      pure s
    FailedSkillTest _iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist | isEasyStandard attrs -> do
          n <- getCurrentAgendaStep
          when (n >= 2) makeInfestationTest
        _ -> pure ()
      pure s
    ScenarioResolution r -> do
      investigators <- allInvestigators
      isFullCampaign <- getIsFullCampaign
      case r of
        NoResolution -> do
          anyResigned <- selectAny ResignedInvestigator
          n <- selectCount InfestedLocation
          steps <- getRecordCount StepsOfTheBridge
          if anyResigned
            then do
              story noResolution
              recordCount StepsOfTheBridge (steps + n)
              record DrMaheswaran'sFateIsUnknown
              record RandolphEscapedTheHospitalOnHisOwn
              addCampaignCardToDeckChoice investigators Assets.randolphCarterChainedToTheWakingWorld
              push R5
            else do
              recordCount StepsOfTheBridge (steps + n)
              push R4
        Resolution 1 -> do
          story resolution1
          record DrMaheswaranIsAlive
          when isFullCampaign $ record TheDreamersGrowWeaker
          record RandolphEscapedTheHospitalWithTheInvestigators
          addCampaignCardToDeckChoice investigators Assets.randolphCarterChainedToTheWakingWorld
          push R5
        Resolution 2 -> do
          story resolution2
          record DrMaheswaranIsMissing
          when isFullCampaign $ record TheDreamersGrowWeaker
          record RandolphEscapedTheHospitalWithTheInvestigators
          addCampaignCardToDeckChoice investigators Assets.randolphCarterChainedToTheWakingWorld
          push R5
        Resolution 3 -> do
          story resolution3
          record DrMaheswaranIsAlive
          record RandolphEscapedTheHospitalWithTheInvestigators
          addCampaignCardToDeckChoice investigators Assets.randolphCarterChainedToTheWakingWorld
          push R5
        Resolution 4 -> do
          story resolution4
          record DrMaheswaranIsMissing
          record RandolphEscapedTheHospitalWithTheInvestigators
          addCampaignCardToDeckChoice investigators Assets.randolphCarterChainedToTheWakingWorld
          push R5
        Resolution 5 -> do
          story resolution5
          allGainXp attrs
          endOfScenario
        _ -> error "Invalid resolution"
      pure s
    _ -> WakingNightmare <$> liftRunMessage msg attrs
