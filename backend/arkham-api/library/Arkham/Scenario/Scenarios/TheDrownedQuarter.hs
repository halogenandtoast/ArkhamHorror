module Arkham.Scenario.Scenarios.TheDrownedQuarter (theDrownedQuarter) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Import.Lifted (setNextCampaignStep)
import Arkham.Campaigns.TheDrownedCity.CampaignSteps (pattern TheApiary, pattern TheWesternWall)
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.ChaosToken
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Xp
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid (Pos (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheDrownedQuarter.Helpers

newtype TheDrownedQuarter = TheDrownedQuarter ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theDrownedQuarter :: Difficulty -> TheDrownedQuarter
theDrownedQuarter difficulty = scenario TheDrownedQuarter "11536" "The Drowned Quarter" difficulty []

instance HasChaosTokenValue TheDrownedQuarter where
  getChaosTokenValue iid chaosTokenFace (TheDrownedQuarter attrs) = case chaosTokenFace of
    Skull -> do
      revealed <- selectCount RevealedLocation
      pure $ ChaosTokenValue Skull (NegativeModifier $ byDifficulty attrs (revealed `div` 2) revealed)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 4 4
    ElderThing -> do
      flooded <- selectAny $ FloodedLocation <> locationWithInvestigator iid
      pure
        $ if flooded then toChaosTokenValue attrs ElderThing 3 4 else toChaosTokenValue attrs ElderThing 5 6
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheDrownedQuarter where
  runMessage msg s@(TheDrownedQuarter attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      headedWest <- getHasRecord TheExpeditionHeadedWest
      storyWithContinue' do
        setTitle "title"
        p "drownedQuarter1"
        p.basic "checkCampaignLog"
        ul do
          li.validate headedWest "headedWest"
          li.validate (not headedWest) "headedEast"

      hasNoPlaceLikeHome <-
        anyM (`investigatorHasTask` Assets.noPlaceLikeHome)
          =<< select (IncludeEliminated Anyone)
      flavor do
        setTitle "title"
        p $ if headedWest then "drownedQuarter2" else "drownedQuarter3"
        p $ if headedWest then "drownedQuarter2Conclusion" else "drownedQuarter3Conclusion"
        ul $ li.validate hasNoPlaceLikeHome "resolveNoPlaceLikeHome"
        p.basic "proceedToSetup"
      pure s
    StandaloneSetup -> do
      setChaosTokens (chaosBagContents attrs.difficulty)
      pure s
    Setup -> runScenarioSetup TheDrownedQuarter attrs do
      setUsesGrid

      gather Set.TheDrownedQuarter
      gather Set.AlienMachinery
      gather Set.CosmicLegacy
      gather Set.DeepOnes
      gather Set.ElderMist
      gather Set.Flood
      gather Set.Rlyeh
      gather Set.UnderseaCreatures

      setActDeck [Acts.reactivateTheCore]
      setAgendaDeck [Agendas.theSunkenRuins, Agendas.collapsingDome]

      -- Grid-based undersea dome with the Barrier Core at the centre; the act
      -- connects each location to every adjacent location.
      -- TODO: place all Sea Floor locations per the placement diagram (Drowned
      -- Acropolis / Blasted Ruins / Coral Reef ×2 [remove one at random] / Ancient
      -- Gallery / Abyssal Trench ×3) and start the investigators at the entrance.
      barrierCore <- placeInGrid (Pos 1 1) Locations.barrierCoreInactive
      startAt barrierCore
    ScenarioResolution res -> scope "resolutions" do
      headedWest <- getHasRecord TheExpeditionHeadedWest
      case res of
        Resolution 1 -> do
          -- You took the Barrier Node artifact.
          record BarrierNode
          resolutionWithXp "resolution1" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        Resolution 2 -> resolutionWithXp "resolution2" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        NoResolution -> do
          record ThePowerWasDiverted
          resolutionWithXp "noResolution" $ allGainXpWithBonus' attrs $ toBonus "bonus" 2
        _ -> error $ "Unknown resolution: " <> show res
      -- TODO: cross out "The Drowned Quarter" on the R'lyeh map (R'lyeh-map recordable).
      if headedWest then setNextCampaignStep TheApiary else setNextCampaignStep TheWesternWall
      endOfScenario
      pure s
    _ -> TheDrownedQuarter <$> liftRunMessage msg attrs
