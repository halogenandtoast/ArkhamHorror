module Arkham.Scenario.Scenarios.RiddlesAndRain (riddlesAndRain) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Meta
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.RiddlesAndRain.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype RiddlesAndRain = RiddlesAndRain ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riddlesAndRain :: Difficulty -> RiddlesAndRain
riddlesAndRain difficulty =
  scenario
    RiddlesAndRain
    "09501"
    "Riddles and Rain"
    difficulty
    [ "hourglass triangle circle"
    , "moon      equals   square"
    , "t         squiggle ."
    ]

instance HasChaosTokenValue RiddlesAndRain where
  getChaosTokenValue iid tokenFace (RiddlesAndRain attrs) = case tokenFace of
    Skull -> do
      clues <- field InvestigatorClues iid
      pure $ toChaosTokenValue attrs Skull (if clues >= 2 then 3 else 1) (if clues >= 2 then 4 else 2)
    Tablet -> pure $ toChaosTokenValue attrs Tablet 1 2
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage RiddlesAndRain where
  runMessage msg s@(RiddlesAndRain attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      setChaosTokens $ campaignChaosBag attrs.difficulty
      pure s
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
      markTime 1
      pure s
    DoStep 7 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro7"
      removeChaosToken Tablet
      addChaosToken ElderThing
      markTime 2
      pure s
    Setup -> runScenarioSetup RiddlesAndRain attrs do
      setup $ ul do
        li "gatherSets"
        li "setSetsOutOfPlay"
        li.nested "placeLocations" do
          li "beginPlay"
          li "remainingLocations"
        li "setOutOfPlay"
        li "miniCards"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

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
    ResolveChaosToken _ Tablet iid -> do
      withLocationOf iid \loc -> do
        anyConcealed <- fieldMap LocationConcealedCards (not . null) loc
        when anyConcealed $ drawAnotherChaosToken iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        ElderThing -> do
          inShadowEnemies <- select $ EnemyWithPlacement InTheShadows
          clues <- field InvestigatorClues iid
          chooseOneM iid do
            targets inShadowEnemies $ placeDoomOn ElderThing 1
            when (clues > 0) $ clueLabeled iid $ spendClues iid 1
        _ -> pure ()
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolution "noResolution"
          push R4
        Resolution 1 -> do
          record YouHaventSeenTheLastOfTheRedGlovedMan
          markTime 1
          resolutionWithXp "resolution1" $ allGainXp' attrs
          chooseBearer Keys.theEyeOfRavens
          endOfScenario
        Resolution 2 -> do
          record YouHaventSeenTheLastOfTheRedGlovedMan
          markTime 1
          resolutionWithXp "resolution2" $ allGainXp' attrs
          chooseBearer Keys.theEyeOfRavens
          endOfScenario
        Resolution 3 -> do
          record YouHaventSeenTheLastOfTheRedGlovedMan
          markTime 1
          setBearer Keys.theEyeOfRavens $ keyWithEnemy Enemies.theRedGlovedManPurposeUnknown
          resolutionWithXp "resolution3" $ allGainXp' attrs
          endOfScenario
        Resolution 4 -> do
          record YouHaventSeenTheLastOfTheRedGlovedMan
          markTime 2
          setBearer Keys.theEyeOfRavens $ keyWithEnemy Enemies.theRedGlovedManPurposeUnknown
          resolutionWithXp "resolution4" $ allGainXp' attrs
          endOfScenario
        _ -> error "invalid resolution"
      pure s
    _ -> RiddlesAndRain <$> liftRunMessage msg attrs
