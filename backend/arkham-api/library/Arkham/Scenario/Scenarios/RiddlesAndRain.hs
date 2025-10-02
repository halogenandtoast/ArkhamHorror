module Arkham.Scenario.Scenarios.RiddlesAndRain (riddlesAndRain) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.RiddlesAndRain.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype RiddlesAndRain = RiddlesAndRain ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riddlesAndRain :: Difficulty -> RiddlesAndRain
riddlesAndRain difficulty = scenario RiddlesAndRain "09501" "Riddles and Rain" difficulty ["equals"]

instance HasChaosTokenValue RiddlesAndRain where
  getChaosTokenValue iid tokenFace (RiddlesAndRain attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage RiddlesAndRain where
  runMessage msg s@(RiddlesAndRain attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro1"
      flavor $ setTitle "title" >> p "letter"
      n <- getPlayerCount
      storyWithChooseOneM' (setTitle "title" >> p "intro1Part2") do
        labeledValidate' (n == 1) "onlyOne" $ doStep 2 msg
        labeledValidate' (n > 1) "goAlone" $ doStep 3 msg
        labeledValidate' (n > 1) "goWithBackup" $ doStep 4 msg

      popScope $ scope "trackingTime" $ flavor $ setTitle "title" >> p "body"
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro2"
      doStep 5 PreScenarioSetup
      pure s
    DoStep 3 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro3"
      doStep 5 PreScenarioSetup
      pure s
    DoStep 4 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro4"
      doStep 5 PreScenarioSetup
      pure s
    DoStep 5 PreScenarioSetup -> scope "intro" do
      storyWithChooseOneM' (setTitle "title" >> p "intro5") do
        labeled' "takeHisOffer" $ doStep 6 PreScenarioSetup
        labeled' "goToLondonOnYourOwnTerms" $ doStep 7 PreScenarioSetup
      pure s
    DoStep 6 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro6"
      removeChaosToken ElderThing
      addChaosToken Tablet
      incrementRecordCount Time 1
      pure s
    DoStep 7 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro7"
      removeChaosToken Tablet
      addChaosToken ElderThing
      incrementRecordCount Time 2
      pure s
    Setup -> runScenarioSetup RiddlesAndRain attrs do
      gather Set.RiddlesAndRain
      gather Set.DarkVeiling
      gather Set.ShadowOfADoubt
      gather Set.StrangeHappenings
      gather Set.ChillingCold
      gather Set.LockedDoors
      gatherJust Set.TheMidnightMasks [Treacheries.falseLead, Treacheries.huntingShadow]

      gatherAndSetAside Set.CrimsonConspiracy
      gatherAndSetAside Set.Outsiders

      setAgendaDeck
        [ Agendas.whenItRains
        , Agendas.figuresInTheFog
        , Agendas.theConnection
        , Agendas.plotsAndPanic
        ]

      setActDeck
        [ Acts.cluesAndCapers
        , Acts.theGameIsAfoot
        , Acts.eyesInTheTower
        , Acts.caughtRedHanded
        ]

      startAt =<< place Locations.rainyLondonStreets
      setAside
        [ Locations.bigBen
        , Locations.westminsterAbbey
        , Locations.kensingtonGardens
        , Locations.theTowerBridge
        , Locations.traitorsGate
        , Locations.towerOfLondon
        , Locations.towerPrison
        , Enemies.theRedGlovedManShroudedInMystery
        , Keys.theEyeOfRavens
        ]
    _ -> RiddlesAndRain <$> liftRunMessage msg attrs
