module Arkham.Scenario.Scenarios.ReturnToExtracurricularActivity (
  ReturnToExtracurricularActivity (..),
  returnToExtracurricularActivity,
) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Campaign
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Prelude
import Arkham.Scenario.Runner
import Arkham.Scenario.Scenarios.ExtracurricularActivity
import Arkham.Scenario.Setup

newtype ReturnToExtracurricularActivity = ReturnToExtracurricularActivity ExtracurricularActivity
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToExtracurricularActivity :: Difficulty -> ReturnToExtracurricularActivity
returnToExtracurricularActivity =
  returnTo
    ReturnToExtracurricularActivity
    "51012"
    "Return to Extracurricular Activity"
    extracurricularActivity

instance HasChaosTokenValue ReturnToExtracurricularActivity where
  getChaosTokenValue iid tokenFace (ReturnToExtracurricularActivity (ExtracurricularActivity attrs)) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage ReturnToExtracurricularActivity where
  runMessage msg (ReturnToExtracurricularActivity base@(ExtracurricularActivity attrs)) = runQueueT $ case msg of
    Setup -> runScenarioSetup (ReturnToExtracurricularActivity . ExtracurricularActivity) attrs do
      gather Set.ReturnToExtracurricularActivity
      gather Set.ExtracurricularActivity
      gather Set.Sorcery
      gather Set.BeyondTheThreshold
      gather Set.BishopsThralls
      gather Set.Whippoorwills
      gather Set.ResurgentEvils
      gather Set.SecretDoors
      gather Set.YogSothothsEmissaries

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

      startAt =<< place Locations.miskatonicQuad
      randomLocation <- traceShowId <$> sample (Locations.orneLibrary :| [Locations.warrenObservatory])
      placeAll
        [ Locations.humanitiesBuilding
        , randomLocation
        , Locations.studentUnion
        , Locations.scienceBuilding
        , Locations.administrationBuilding
        ]
    _ -> ReturnToExtracurricularActivity <$> liftRunMessage msg base
