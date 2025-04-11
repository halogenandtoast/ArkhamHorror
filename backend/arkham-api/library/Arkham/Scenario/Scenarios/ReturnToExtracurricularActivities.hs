module Arkham.Scenario.Scenarios.ReturnToExtracurricularActivities (returnToExtracurricularActivities) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Campaign hiding (addCampaignCardToDeckChoice)
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.ExtracurricularActivity
import Arkham.Scenarios.ExtracurricularActivity.Helpers
import Data.Bifoldable

newtype ReturnToExtracurricularActivities = ReturnToExtracurricularActivities ExtracurricularActivity
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToExtracurricularActivities :: Difficulty -> ReturnToExtracurricularActivities
returnToExtracurricularActivities difficulty =
  scenarioWith
    (ReturnToExtracurricularActivities . ExtracurricularActivity)
    "51012"
    "Return to Extracurricular Activities"
    difficulty
    [ "triangle plus    hourglass squiggle"
    , "square   diamond circle    ."
    , ".        equals  t         ."
    ]
    (referenceL .~ "02041")

instance RunMessage ReturnToExtracurricularActivities where
  runMessage msg s@(ReturnToExtracurricularActivities extracurricularActivity'@(ExtracurricularActivity attrs)) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro1"
      pure s
    Setup -> runScenarioSetup (ReturnToExtracurricularActivities . ExtracurricularActivity) attrs do
      gather Set.ReturnToExtracurricularActivities
      gather Set.ExtracurricularActivity
      gather Set.Sorcery
      gather Set.BeyondTheThreshold
      gather Set.BishopsThralls
      gather Set.Whippoorwills
      gather Set.ResurgentEvils
      gather Set.SecretDoors
      gather Set.YogSothothsEmissaries

      startAt =<< place Locations.miskatonicQuad
      placeAll
        [ Locations.humanitiesBuilding
        , Locations.studentUnion
        , Locations.scienceBuilding
        ]

      administrationBuilding <- place Locations.administrationBuilding
      result <- sampleWithRest (Locations.orneLibrary :| [Locations.warrenObservatory])
      bifor_ result place_ removeEvery

      completedTheHouseAlwaysWins <- elem "51015" <$> getCompletedScenarios
      setAside
        [ if completedTheHouseAlwaysWins
            then Locations.facultyOfficesTheHourIsLate
            else Locations.facultyOfficesTheNightIsStillYoung
        , Assets.jazzMulligan
        , Assets.alchemicalConcoction
        , Enemies.theExperiment
        , Locations.dormitories
        , Locations.alchemyLabs
        , Assets.professorWarrenRice
        ]

      when completedTheHouseAlwaysWins do
        enemyAt_ Enemies.enthralledSecurityGuard administrationBuilding

      setAgendaDeck [Agendas.quietHalls, Agendas.deadOfNight, Agendas.theBeastUnleashed]
      setActDeck [Acts.afterHours, Acts.ricesWhereabouts, Acts.campusSafety]
    _ -> ReturnToExtracurricularActivities <$> liftRunMessage msg extracurricularActivity'
