module Arkham.Scenario.Scenarios.ObsidianCanyons (obsidianCanyons) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Import.Lifted (setNextCampaignStep)
import Arkham.Campaigns.TheDrownedCity.CampaignSteps (pattern CourtOfTheAncients, pattern SepulchreOfTheSleeper)
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
import Arkham.Scenarios.ObsidianCanyons.Helpers

newtype ObsidianCanyons = ObsidianCanyons ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

obsidianCanyons :: Difficulty -> ObsidianCanyons
obsidianCanyons difficulty = scenario ObsidianCanyons "11639" "Obsidian Canyons" difficulty []

instance HasChaosTokenValue ObsidianCanyons where
  getChaosTokenValue iid chaosTokenFace (ObsidianCanyons attrs) = case chaosTokenFace of
    Skull -> do
      storm <- getStormIntensity
      pure $ ChaosTokenValue Skull (NegativeModifier $ byDifficulty attrs storm (storm + 2))
    Cultist -> pure $ toChaosTokenValue attrs Cultist 4 6
    -- TODO: tablet is -1/-2 for EACH open sky adjacent to your location.
    Tablet -> pure $ toChaosTokenValue attrs Tablet 1 2
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 2 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage ObsidianCanyons where
  runMessage msg s@(ObsidianCanyons attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      setChaosTokens (chaosBagContents attrs.difficulty)
      pure s
    Setup -> runScenarioSetup ObsidianCanyons attrs do
      headedWest <- getHasRecord TheExpeditionHeadedWest
      gather Set.ObsidianCanyons
      gather Set.ChillingCold
      gather Set.StrikingFear

      if headedWest
        then do
          setActDeck [Acts.searchingTheSpires, Acts.toTheAncientDome]
          setAgendaDeck [Agendas.otherworldlyStorms]
        else do
          setActDeck [Acts.scouringTheSpires, Acts.deadlySkies, Acts.returnToTheShoreline]
          setAgendaDeck [Agendas.encroachingStorms]

      -- The sky-city is a grid of floating Summit locations with "open sky" cards
      -- (the Chilling Cold + Striking Fear sets) filling the gaps; locations slide in
      -- and out from the Summit deck.
      -- TODO: build the Summit deck, place R'lyeh Streets + its ring of four open-sky
      -- cards per the page-35 diagram, set aside Central Spire/etc., and (east only)
      -- keep Western Wall/Floating Spire. Star/glyph set-aside cards as needed.
      rlyehStreets <- place Locations.rlyehStreets
      startAt rlyehStreets

      -- Storm Intensity starts at 1.
      setScenarioMeta initObsidianMeta
    ScenarioSpecific "increaseStormIntensity" _ -> do
      storm <- getStormIntensity
      setScenarioMeta (ObsidianMeta (storm + 1))
      pure s
    ScenarioSpecific "decreaseStormIntensity" _ -> do
      storm <- getStormIntensity
      setScenarioMeta (ObsidianMeta (max 0 (storm - 1)))
      pure s
    ScenarioResolution res -> scope "resolutions" do
      headedWest <- getHasRecord TheExpeditionHeadedWest
      -- Obsidian Claw is earned if an investigator still controls it at the end.
      whenM (selectAny $ assetIs Assets.obsidianClaw) $ record ObsidianClaw
      case res of
        Resolution 1 -> resolutionWithXp "resolution1" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        Resolution 2 -> resolutionWithXp "resolution2" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        NoResolution -> resolutionWithXp "noResolution" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        _ -> error $ "Unknown resolution: " <> show res
      -- TODO: cross out "Obsidian Canyons" on the R'lyeh map (R'lyeh-map recordable).
      if headedWest then setNextCampaignStep SepulchreOfTheSleeper else setNextCampaignStep CourtOfTheAncients
      endOfScenario
      pure s
    _ -> ObsidianCanyons <$> liftRunMessage msg attrs
