module Arkham.Scenario.Scenarios.TheHouseAlwaysWins (theHouseAlwaysWins, TheHouseAlwaysWins (..)) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheDunwichLegacy.ChaosBag
import Arkham.Campaigns.TheDunwichLegacy.Key
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Exception
import Arkham.Helpers.Cost
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query
import Arkham.Helpers.Xp
import Arkham.Location.Cards qualified as Locations
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Modifier
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheHouseAlwaysWins.Helpers

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
    [ ".    .      .        backHallDoorway1 ."
    , ".    .      triangle backHallDoorway1 ."
    , "moon circle triangle diamond          backHallDoorway2"
    , "moon circle square   diamond          backHallDoorway2"
    , ".    .      square   backHallDoorway3 ."
    , ".    .      .        backHallDoorway3 ."
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
      flavor $ scope "intro" do
        h "title"
        p "body"
      pure s
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    Setup -> runScenarioSetup TheHouseAlwaysWins attrs do
      setup do
        ul do
          li "gatherSets"
          li "setAsideEncounterSets"
          li "placeLocations"
          li "placeCloverClubPitBoss"
          li "setAside"
          unscoped $ li "shuffleRemainder"
        p "note"

      gather Set.TheHouseAlwaysWins
      gather Set.BadLuck
      gather Set.NaomisCrew
      gather Set.Rats
      gatherAndSetAside Set.HideousAbominations
      gatherAndSetAside Set.StrikingFear

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
    PassedSkillTestWithToken iid Cultist | isEasyStandard attrs -> do
      gainResourcesIfCan iid Cultist 3
      pure s
    FailedSkillTestWithToken iid Cultist | isHardExpert attrs -> do
      spendResources iid 3
      pure s
    FailedSkillTestWithToken iid Tablet | isEasyStandard attrs -> do
      spendResources iid 3
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> push R1
        Resolution 1 -> do
          resolutionWithXp "resolution1" $ allGainXpWithBonus' attrs $ toBonus "resolution1" 1
          record OBannionGangHasABoneToPickWithTheInvestigators
          record DrFrancisMorganWasKidnapped
          unlessM (null <$> cheated) $ addChaosToken ElderThing
          endOfScenario
        Resolution 2 -> do
          resolutionWithXp "resolution2" $ allGainXp' attrs
          record OBannionGangHasABoneToPickWithTheInvestigators
          record TheInvestigatorsRescuedDrFrancisMorgan
          investigatorIds <- allInvestigators
          addCampaignCardToDeckChoice investigatorIds DoNotShuffleIn Assets.drFrancisMorgan
          unlessM (null <$> cheated) $ addChaosToken ElderThing
          endOfScenario
        Resolution 3 -> do
          resolutionWithXp "resolution3" $ allGainXp' attrs
          record NaomiHasTheInvestigatorsBacks
          record DrFrancisMorganWasKidnapped
          unlessM (null <$> cheated) $ addChaosToken ElderThing
          endOfScenario
        Resolution 4 -> do
          resolutionWithXp "resolution4" $ allGainXpWithBonus' attrs $ toBonus "resolution4" 1
          record OBannionGangHasABoneToPickWithTheInvestigators
          record DrFrancisMorganWasKidnapped
          record InvestigatorsWereUnconsciousForSeveralHours
          eachInvestigator (`sufferPhysicalTrauma` 1)
          unlessM (null <$> cheated) $ addChaosToken ElderThing
          endOfScenario
        other -> throwIO $ UnknownResolution other
      pure s
    _ -> TheHouseAlwaysWins <$> liftRunMessage msg attrs
