module Arkham.Scenario.Scenarios.SepulchreOfTheSleeper (sepulchreOfTheSleeper) where

import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaign.Import.Lifted (setNextCampaignStep)
import Arkham.Campaigns.TheDrownedCity.CampaignSteps (pattern TheAwakening)
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.ChaosToken
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Xp
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Message.Lifted.Log (record)
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.SepulchreOfTheSleeper.Helpers

newtype SepulchreOfTheSleeper = SepulchreOfTheSleeper ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

sepulchreOfTheSleeper :: Difficulty -> SepulchreOfTheSleeper
sepulchreOfTheSleeper difficulty = scenario SepulchreOfTheSleeper "11673" "Sepulchre of the Sleeper" difficulty []

instance HasChaosTokenValue SepulchreOfTheSleeper where
  getChaosTokenValue iid chaosTokenFace (SepulchreOfTheSleeper attrs) = case chaosTokenFace of
    Skull -> do
      disturbance <- getDisturbance
      pure $ ChaosTokenValue Skull (NegativeModifier $ byDifficulty attrs (min 6 disturbance) disturbance)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 4 6
    Tablet -> pure $ toChaosTokenValue attrs Tablet 4 6
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage SepulchreOfTheSleeper where
  runMessage msg s@(SepulchreOfTheSleeper attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      setChaosTokens (chaosBagContents attrs.difficulty)
      pure s
    Setup -> runScenarioSetup SepulchreOfTheSleeper attrs do
      gather Set.SepulchreOfTheSleeper
      gather Set.Domination
      gather Set.Dreams
      gather Set.Rlyeh
      gather Set.StarSpawn
      gather Set.AncientEvils
      gather Set.StrikingFear

      -- A special two-agenda deck (no acts): the Sleeper rouses, then awakens.
      setAgendaDeck [Agendas.beneathTheCity, Agendas.cthulhuAwakened]

      dreamersRest <- place Locations.dreamersRest
      placeAll
        [ Locations.sigilCarvedAlcoveStoryOfAmbition
        , Locations.sigilCarvedAlcoveStoryOfResilience
        , Locations.sigilCarvedAlcoveStoryOfInfinity
        , Locations.sigilCarvedAlcoveStoryOfDefiance
        , Locations.sigilCarvedAlcoveStoryOfTheVoyage
        ]
      startAt dreamersRest

      -- Cthulhu (Dead and Dreaming) sleeps at the centre.
      createEnemyAt_ Enemies.cthulhuDeadAndDreaming dreamersRest

      -- TODO: put each earned Artifact asset into play (divided among investigators);
      -- in player order each player may take 1 Item from the Expedition set.
      setScenarioMeta initSepulchreMeta
    ScenarioSpecific "increaseDisturbance" _ -> do
      disturbance <- getDisturbance
      setScenarioMeta (SepulchreMeta (disturbance + 1))
      pure s
    ScenarioResolution res -> scope "resolutions" do
      case res of
        Resolution 1 -> resolutionWithXp "resolution1" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        NoResolution -> do
          -- Each surviving investigator is driven insane.
          record CthulhuAnnihilatedTheExpedition
          resolutionWithXp "noResolution" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        _ -> error $ "Unknown resolution: " <> show res
      -- Both paths converge on the Awakening; the campaign finale (Return to Arkham →
      -- The Doom of Arkham) follows from there.
      setNextCampaignStep TheAwakening
      endOfScenario
      pure s
    _ -> SepulchreOfTheSleeper <$> liftRunMessage msg attrs
