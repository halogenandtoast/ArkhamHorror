module Arkham.Scenario.Scenarios.RiverOfBlood (riverOfBlood) where

import Arkham.Agenda.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Agendas
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Matcher
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.RiverOfBlood.Helpers
import Arkham.Trait (Trait (Dawn, Dusk))

newtype RiverOfBlood = RiverOfBlood ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riverOfBlood :: Difficulty -> RiverOfBlood
riverOfBlood difficulty = scenario RiverOfBlood "13001" "River of Blood" difficulty []

instance HasChaosTokenValue RiverOfBlood where
  getChaosTokenValue iid tokenFace (RiverOfBlood attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage RiverOfBlood where
  runMessage msg s@(RiverOfBlood attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro"
      pure s
    Setup -> runScenarioSetup RiverOfBlood attrs do
      setup $ ul do
        li "gatherSets"
        li "gatherAdditionalSets"
        li.nested "placeLocations" do
          li "dawn"
          li "dusk"
          li "startAt"
        li.nested "waterfrontCivilians" do
          li "additionalWaterfrontCivilians"
        li.nested "juliaStern" do
          li "onTheRun"
          li "stalkingTheStreets"
          li "preyingUponArkham"
        li "setOutOfPlay"
        unscoped $ li "shuffleRemainder"
        li "lairs"
        unscoped $ li "readyToBegin"

      n <- getPlayerCount
      gather Set.RiverOfBlood
      gather Set.BloodBlight
      gather Set.BloodMoon
      gather Set.ChildrenOfBlood
      gather Set.Hunted
      gather Set.Infected
      gather Set.Misinformation
      if isEasyStandard attrs
        then gather Set.PreyedUpon
        else do
          gather Set.AgentsOfZburamoarte
          gather Set.Mongrels
      gather Set.Vermin
      gather Set.DeadEnds
      gather Set.FlyingTerrors
      when (n >= 3) $ gather Set.Afflicted

      setAgendaDeck
        [Agendas.theFirstDay, Agendas.theFirstNight, Agendas.theSecondDay, Agendas.theSecondNight]

      if isEasyStandard attrs
        then removeCards =<< amongGathered (#location <> CardWithTrait Dusk)
        else removeCards =<< amongGathered (#location <> CardWithTrait Dawn)
    _ -> RiverOfBlood <$> liftRunMessage msg attrs
