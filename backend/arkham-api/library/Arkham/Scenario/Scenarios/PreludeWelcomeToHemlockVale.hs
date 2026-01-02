module Arkham.Scenario.Scenarios.PreludeWelcomeToHemlockVale (preludeWelcomeToHemlockVale) where

import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.EncounterSet qualified as Set
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenario.Import.Lifted
import Arkham.Helpers.FlavorText
import Arkham.Message.Lifted.Choose

newtype PreludeWelcomeToHemlockVale = PreludeWelcomeToHemlockVale ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

preludeWelcomeToHemlockVale :: Difficulty -> PreludeWelcomeToHemlockVale
preludeWelcomeToHemlockVale difficulty =
  scenario
    PreludeWelcomeToHemlockVale
    "10675a"
    "Prelude: Welcome to Hemlock Vale"
    difficulty
    []

instance HasChaosTokenValue PreludeWelcomeToHemlockVale where
  getChaosTokenValue iid tokenFace (PreludeWelcomeToHemlockVale attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage PreludeWelcomeToHemlockVale where
  runMessage msg s@(PreludeWelcomeToHemlockVale attrs) = runQueueT $ campaignI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      storyWithChooseOneM' (h "title" >> p "intro1") do
        labeled' "survey" $ doStep 2 PreScenarioSetup
        labeled' "feast" $ doStep 3 PreScenarioSetup
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "intro2"
      doStep 4 PreScenarioSetup
      pure s
    DoStep 3 PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "intro3"
      doStep 4 PreScenarioSetup
      pure s
    DoStep 4 PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "intro4"
      pure s
    Setup -> runScenarioSetup PreludeWelcomeToHemlockVale attrs do
      gather Set.TheFirstDay
      gather Set.DayOfRest
      gather Set.Residents
      gather Set.TheVale

      startAt =<< place Locations.theCrossroadsDay

      placeAll
        [ Locations.boardingHouseDay
        , Locations.hemlockChapelDay
        , Locations.theOldMillDay
        , Locations.theAtwoodHouseDay
        , Locations.tadsGeneralStoreDay
        , Locations.valeSchoolhouseDay
        , Locations.theCommonsDay
        ]
    _ -> PreludeWelcomeToHemlockVale <$> liftRunMessage msg attrs
