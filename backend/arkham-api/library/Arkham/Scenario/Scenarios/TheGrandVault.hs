module Arkham.Scenario.Scenarios.TheGrandVault (theGrandVault) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Import.Lifted (setNextCampaignStep)
import Arkham.Campaigns.TheDrownedCity.CampaignSteps (pattern CourtOfTheAncients, pattern TheApiary)
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.ChaosToken
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.Xp
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheGrandVault.Helpers

newtype TheGrandVault = TheGrandVault ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theGrandVault :: Difficulty -> TheGrandVault
theGrandVault difficulty = scenario TheGrandVault "11587" "The Grand Vault" difficulty []

instance HasChaosTokenValue TheGrandVault where
  getChaosTokenValue iid chaosTokenFace (TheGrandVault attrs) = case chaosTokenFace of
    Skull -> do
      activated <- getActivatedCount
      pure $ ChaosTokenValue Skull (NegativeModifier $ byDifficulty attrs (activated `div` 2) activated)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 3 4
    -- TODO: tablet scales with the investigator's location flood (-2/-3/-4 easy, -3/-4/-5 hard).
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 5
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheGrandVault where
  runMessage msg s@(TheGrandVault attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      setChaosTokens (chaosBagContents attrs.difficulty)
      pure s
    Setup -> runScenarioSetup TheGrandVault attrs do
      gather Set.TheGrandVault
      gather Set.AlienMachinery
      gather Set.Flood
      gather Set.TheInescapable
      gather Set.Rlyeh
      gather Set.StarSpawn

      setActDeck [Acts.carefulNavigation, Acts.backThroughTheMachine]
      setAgendaDeck [Agendas.bowelsOfTheCity, Agendas.devilInTheMachine, Agendas.everShiftingWalls]

      -- The vault is a grid navigated only via the Moving Platform; locations are
      -- connected solely by their connection icons or the Platform.
      -- TODO: place the four fixed locations and the shuffled Vault Chamber ring per
      -- the page-36 diagram; flood the bottom-row Vault Chambers; Star Spawn cull
      -- (remove 2 at random, set the rest aside); Toe the Line pre-activation.
      greatStair <- place Locations.theGreatStair
      placeAll [Locations.movingPlatformObservationStation, Locations.coreOfTheVaultHeartOfTheMachine]
      startAt greatStair

      -- Set-aside cards brought in by acts/agendas/effects.
      setAside
        [ Assets.tidalTablet
        , Locations.chamberOfTheTabletUnsealed
        ]
    ScenarioResolution res -> scope "resolutions" do
      headedWest <- getHasRecord TheExpeditionHeadedWest
      -- Tidal Tablet is earned if an investigator still controls it at the end.
      whenM (selectAny $ assetIs Assets.tidalTablet) $ record TidalTablet
      case res of
        Resolution 1 -> resolutionWithXp "resolution1" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        Resolution 2 -> resolutionWithXp "resolution2" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        NoResolution -> resolutionWithXp "noResolution" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        _ -> error $ "Unknown resolution: " <> show res
      -- TODO: cross out "The Grand Vault" on the R'lyeh map (R'lyeh-map recordable).
      if headedWest then setNextCampaignStep CourtOfTheAncients else setNextCampaignStep TheApiary
      endOfScenario
      pure s
    _ -> TheGrandVault <$> liftRunMessage msg attrs
