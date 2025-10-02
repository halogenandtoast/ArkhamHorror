module Arkham.Scenario.Scenarios.RiddlesAndRain (riddlesAndRain) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
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
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro1"
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
