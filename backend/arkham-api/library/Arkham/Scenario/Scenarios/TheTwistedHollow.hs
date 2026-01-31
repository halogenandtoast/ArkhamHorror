module Arkham.Scenario.Scenarios.TheTwistedHollow (theTwistedHollow) where

import Arkham.Campaigns.TheFeastOfHemlockVale.Key
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.FlavorText
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheTwistedHollow.Helpers
import Arkham.Story.Cards qualified as Stories

newtype TheTwistedHollow = TheTwistedHollow ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTwistedHollow :: Difficulty -> TheTwistedHollow
theTwistedHollow difficulty = scenario TheTwistedHollow "10605" "The Twisted Hollow" difficulty []

instance HasChaosTokenValue TheTwistedHollow where
  getChaosTokenValue iid tokenFace (TheTwistedHollow attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheTwistedHollow where
  runMessage msg s@(TheTwistedHollow attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      storyWithChooseOneM' (setTitle "title" >> p "intro1") do
        labeled' "tellTheTruth" $ doStep 2 PreScenarioSetup
        labeled' "lie" $ doStep 3 PreScenarioSetup
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      record MotherRachelShowedTheWay
      addChaosToken #cultist
      flavor $ setTitle "title" >> p "intro2"
      pure s
    DoStep 3 PreScenarioSetup -> scope "intro" do
      record TheInvestigatorsLostThePath
      addChaosToken #elderthing
      flavor $ setTitle "title" >> p "intro3"
      pure s
    Setup -> runScenarioSetup TheTwistedHollow attrs do
      setup $ ul do
        li "gatherSets"
        li "night"
        li.nested "checkCampaignLog" do
          li "showedTheWay"
          li "lostThePath"
        li.nested "removeLocations" do
          li "oneOrTwoInvestigators"
          li "threeOrFourInvestigators"

      gather Set.TheFirstDay
      gather Set.TheTwistedHollow
      gather Set.TheForest
      gather Set.Myconids

      placeStory Stories.nightOne
    _ -> TheTwistedHollow <$> liftRunMessage msg attrs
