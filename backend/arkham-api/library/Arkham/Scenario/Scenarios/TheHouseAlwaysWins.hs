module Arkham.Scenario.Scenarios.TheHouseAlwaysWins (TheHouseAlwaysWins (..), theHouseAlwaysWins) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheDunwichLegacy.ChaosBag
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Message.Lifted.Choose
import Arkham.Resolution
import Arkham.Scenario.Helpers hiding (addCampaignCardToDeckChoice, chaosTokenEffect)
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheHouseAlwaysWins.Helpers
import Arkham.Scenarios.TheHouseAlwaysWins.Story

newtype TheHouseAlwaysWins = TheHouseAlwaysWins ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theHouseAlwaysWins :: Difficulty -> TheHouseAlwaysWins
theHouseAlwaysWins difficulty =
  scenario
    TheHouseAlwaysWins
    "02062"
    "The House Always Wins"
    difficulty
    [ ".           .                .                  backHallDoorway1 ."
    , ".           .                cloverClubCardroom backHallDoorway1 ."
    , "laBellaLuna cloverClubLounge cloverClubCardroom darkenedHall     backHallDoorway2"
    , "laBellaLuna cloverClubLounge cloverClubBar      darkenedHall     backHallDoorway2"
    , ".           .                cloverClubBar      backHallDoorway3 ."
    , ".           .                .                  backHallDoorway3 ."
    ]

instance HasChaosTokenValue TheHouseAlwaysWins where
  getChaosTokenValue iid chaosTokenFace (TheHouseAlwaysWins attrs) = case chaosTokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 2 3
    Cultist -> pure $ ChaosTokenValue Cultist (NegativeModifier 3)
    Tablet -> pure $ ChaosTokenValue Tablet (NegativeModifier 2)
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheHouseAlwaysWins where
  runMessage msg s@(TheHouseAlwaysWins attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story intro
      pure s
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    Setup -> runScenarioSetup TheHouseAlwaysWins attrs do
      gather Set.TheHouseAlwaysWins
      gather Set.BadLuck
      gather Set.NaomisCrew
      gather Set.Rats

      startAt =<< place Locations.laBellaLuna
      cloverClubLounge <- place Locations.cloverClubLounge
      placeAll [Locations.cloverClubBar, Locations.cloverClubCardroom]
      enemyAt_ Enemies.cloverClubPitBoss cloverClubLounge

      setAside
        [ Locations.darkenedHall
        , Assets.peterClover
        , Assets.drFrancisMorgan
        , Locations.artGallery
        , Locations.vipArea
        , Locations.backAlley
        ]

      setActDeck [Acts.beginnersLuck, Acts.skinGame, Acts.allIn, Acts.fold]
      setAgendaDeck [Agendas.theCloverClub, Agendas.undergroundMuscle, Agendas.chaosInTheCloverClub]
    ResolveChaosToken _ Tablet iid -> s <$ push (SpendResources iid 3)
    ResolveChaosToken drawnToken Skull iid -> do
      let requiredResources = if isEasyStandard attrs then 2 else 3
      resourceCount <- getSpendableResources iid
      when (resourceCount >= requiredResources) do
        chooseOneM iid do
          labeled ("Spend " <> tshow requiredResources <> " resources to treat this token as a 0") do
            push $ SpendResources iid requiredResources
            chaosTokenEffect Skull drawnToken $ ChaosTokenFaceModifier [Zero]
          labeled "Do not spend resources" nothing
      pure s
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist | isEasyStandard attrs -> gainResourcesIfCan iid Cultist 3
        _ -> pure ()
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist | isHardExpert attrs -> push $ SpendResources iid 3
        Tablet | isEasyStandard attrs -> push $ SpendResources iid 3
        _ -> pure ()
      pure s
    ScenarioResolution NoResolution -> do
      push R1
      pure s
    ScenarioResolution (Resolution 1) -> do
      story resolution1
      record OBannionGangHasABoneToPickWithTheInvestigators
      record DrFrancisMorganWasKidnapped
      when (Cheated `member` attrs.log) $ addChaosToken ElderThing
      allGainXpWithBonus attrs $ toBonus "resolution1" 1
      endOfScenario
      pure s
    ScenarioResolution (Resolution 2) -> do
      investigatorIds <- allInvestigators
      story resolution2
      record OBannionGangHasABoneToPickWithTheInvestigators
      record TheInvestigatorsRescuedDrFrancisMorgan
      addCampaignCardToDeckChoice investigatorIds Assets.drFrancisMorgan
      when (Cheated `member` attrs.log) $ addChaosToken ElderThing
      allGainXp attrs
      endOfScenario
      pure s
    ScenarioResolution (Resolution 3) -> do
      story resolution3
      record NaomiHasTheInvestigatorsBacks
      record DrFrancisMorganWasKidnapped
      when (Cheated `member` attrs.log) $ addChaosToken ElderThing
      allGainXp attrs
      endOfScenario
      pure s
    ScenarioResolution (Resolution 4) -> do
      story resolution4
      record OBannionGangHasABoneToPickWithTheInvestigators
      record DrFrancisMorganWasKidnapped
      record InvestigatorsWereUnconsciousForSeveralHours
      eachInvestigator (`sufferPhysicalTrauma` 1)
      when (Cheated `member` attrs.log) $ addChaosToken ElderThing
      allGainXpWithBonus attrs $ toBonus "resolution4" 1
      endOfScenario
      pure s
    _ -> TheHouseAlwaysWins <$> liftRunMessage msg attrs
