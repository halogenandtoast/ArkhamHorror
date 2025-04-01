module Arkham.Scenario.Scenarios.TheHeartOfMadness (theHeartOfMadness) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.Campaigns.EdgeOfTheEarth.Supplies
import Arkham.EncounterSet qualified as Set
import Arkham.FlavorText
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Text
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheHeartOfMadness.Helpers

newtype TheHeartOfMadness = TheHeartOfMadness ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHeartOfMadness :: Difficulty -> TheHeartOfMadness
theHeartOfMadness difficulty =
  scenario
    TheHeartOfMadness
    "08648"
    "The Heart of Madness"
    difficulty
    []

instance HasChaosTokenValue TheHeartOfMadness where
  getChaosTokenValue iid tokenFace (TheHeartOfMadness attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheHeartOfMadness where
  runMessage msg s@(TheHeartOfMadness attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro"
      kenslerIsAlive <- getPartnerIsAlive Assets.drAmyKenslerProfessorOfBiology
      blueStory
        $ validateEntry kenslerIsAlive "kensler.alive"
        <> hr
        <> validateEntry (not kenslerIsAlive) "kensler.otherwise"

      unless kenslerIsAlive do
        eachInvestigator (`sufferPhysicalTrauma` 1)

      scoutedTheForkedPass <- getHasRecord TheInvestigatorsScoutedTheForkedPass
      blueStory
        $ validateEntry scoutedTheForkedPass "scoutedTheForkedPass.yes"
        <> hr
        <> validateEntry (not scoutedTheForkedPass) "scoutedTheForkedPass.no"

      danforthIsAlive <- getPartnerIsAlive Assets.danforthBrilliantStudent
      story
        $ i18n "hoursPass"
        <> blueFlavor
          ( validateEntry danforthIsAlive "danforth.alive"
              <> hr
              <> validateEntry (not danforthIsAlive) "danforth.otherwise"
          )

      unless danforthIsAlive do
        eachInvestigator (`sufferMentalTrauma` 1)

      miasmicCrystalRecovered <- hasSupply MiasmicCrystal
      blueStory
        $ validateEntry miasmicCrystalRecovered "miasmicCrystal.recovered"
        <> hr
        <> validateEntry (not miasmicCrystalRecovered) "miasmicCrystal.unrecovered"

      unless miasmicCrystalRecovered do
        whenM hasRemainingFrostTokens $ addChaosToken #frost

      storyWithChooseOneM (i18nWithTitle "proceed") do
        labeled
          "Stay here and study the great door to learn more. You will play both parts of the scenario. Proceed to The Heart of Madness, Part 1."
          $ doStep 1 msg
        labeled
          "There is no time to waste. Pass through the gate! You will skip the first part of the scenario. Skip directly to The Heart of Madness, Part 2."
          $ doStep 2 msg

      pure s
    Setup -> runScenarioSetup TheHeartOfMadness attrs do
      gather Set.TheHeartOfMadness
    _ -> TheHeartOfMadness <$> liftRunMessage msg attrs
