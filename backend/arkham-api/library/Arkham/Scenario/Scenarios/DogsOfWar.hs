module Arkham.Scenario.Scenarios.DogsOfWar (dogsOfWar) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.FlavorText
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.DogsOfWar.Helpers
import Arkham.Trait (Trait (Miskatonic, Scholar))

newtype DogsOfWar = DogsOfWar ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dogsOfWar :: Difficulty -> DogsOfWar
dogsOfWar difficulty =
  scenario
    DogsOfWar
    "09635"
    "Dogs of War"
    difficulty
    []

data Version = Version1 | Version2 | Version3
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance HasChaosTokenValue DogsOfWar where
  getChaosTokenValue iid tokenFace (DogsOfWar attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage DogsOfWar where
  runMessage msg s@(DogsOfWar attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      t <- getTime
      flavor do
        setTitle "title"
        p "intro1"
        ul do
          li.validate (t < 20) "fewerThanTwentyTime"
          li.validate (t >= 20) "twentyOrMoreTime"
      doStep (if t < 20 then 2 else 3) PreScenarioSetup
      setupKeys
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      miskatonicOrScholar <- selectAny $ mapOneOf InvestigatorWithTrait [Miskatonic, Scholar]
      storyWithChooseOneM'
        do
          setTitle "title"
          p "intro2Part1"
          p.validate miskatonicOrScholar "miskatonicOrScholar"
          p "intro2Part2"
        do
          labeled' "weAccept" $ doStep 4 PreScenarioSetup
          labeled' "refuse" $ doStep 5 PreScenarioSetup
      pure s
    DoStep 3 PreScenarioSetup -> scope "intro" do
      storyWithChooseOneM' (setTitle "title" >> p "intro3") do
        labeled' "weAccept" $ doStep 6 PreScenarioSetup
        labeled' "refuse" $ doStep 7 PreScenarioSetup
      pure s
    DoStep 4 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro4"
      pure $ DogsOfWar $ attrs & setMetaKey "version" Version1
    DoStep 5 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro5"
      pure $ DogsOfWar $ attrs & setMetaKey "version" Version2
    DoStep 6 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro6"
      pure $ DogsOfWar $ attrs & setMetaKey "version" Version3
    DoStep 7 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro7"
      pure $ DogsOfWar $ attrs & setMetaKey "version" Version3
    Setup -> do
      case getMetaKeyDefault "version" Version1 attrs of
        Version1 -> doStep 1 Setup
        Version2 -> doStep 3 Setup
        Version3 -> doStep 3 Setup
      pure s
    DoStep 1 Setup -> runScenarioSetup DogsOfWar attrs do
      scope "version1" do
        setup $ ul do
          li "gatherSets"
          li.nested "placeLocations" do
            li "startAt"
          li "agendaDeck"
          li "actDeck"
          li.nested "keyLocus" do
            li "doom"
          li "otherLocations"
          li "theClaretKnight"
          li "theBeast"
          unscoped $ li "shuffleRemainder"
          unscoped $ li "readyToBegin"
      gather Set.DogsOfWar
      gather Set.ScarletSorcery
      gather Set.SpatialAnomaly
      gather Set.SpreadingCorruption
      gather Set.DarkCult
      setAgendaDeck [Agendas.brewingCatastropheV1]
      setActDeck [Acts.rabbitsWhoRunV1]
    -- startAt =<< place Locations.qaitbayCitadel
    -- placeAll
    --   [ Locations.windsorPalaceHotel
    --   , Locations.victoriaCollege
    --   , Locations.theCorniche
    --   , Locations.zanEtElSettat
    --   ]
    DoStep 2 Setup -> runScenarioSetup DogsOfWar attrs do
      scope "version2" do
        setup $ ul do
          li "gatherSets"
          li.nested "placeLocations" do
            li "startAt"
          li "agendaDeck"
          li "actDeck"
          li "keyLocus"
          li "otherLocations"
          li "theClaretKnight"
          li "theBeast"
          li "miniCards"
          li "coterieAssassin"
          unscoped $ li "shuffleRemainder"
          unscoped $ li "readyToBegin"
      gather Set.DogsOfWar
      gather Set.CleanupCrew
      gather Set.ScarletSorcery
      gather Set.SpatialAnomaly
      gather Set.SpreadingCorruption
      setAgendaDeck [Agendas.brewingCatastropheV2]
      setActDeck [Acts.rabbitsWhoRunV2]
    DoStep 3 Setup -> runScenarioSetup DogsOfWar attrs do
      scope "version3" do
        setup $ ul do
          li "gatherSets"
          li.nested "placeLocations" do
            li "startAt"
          li "agendaDeck"
          li "actDeck"
          li "keyLocus"
          li "otherLocations"
          li "theBeast"
          li "theClaretKnight"
          li "miniCards"
          unscoped $ li "shuffleRemainder"
          unscoped $ li "readyToBegin"
      gather Set.DogsOfWar
      gather Set.CleanupCrew
      gather Set.ScarletSorcery
      gather Set.SpatialAnomaly
      gather Set.SpreadingCorruption
      gather Set.DarkCult
      setAgendaDeck [Agendas.brewingCatastropheV3]
      setActDeck [Acts.rabbitsWhoRunV3]
    _ -> DogsOfWar <$> liftRunMessage msg attrs
