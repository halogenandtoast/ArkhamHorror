module Arkham.Scenario.Scenarios.TheWesternWall (theWesternWall) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaign.Import.Lifted (setNextCampaignStep)
import Arkham.Campaigns.TheDrownedCity.CampaignSteps (
  pattern SepulchreOfTheSleeper,
  pattern TheDrownedQuarter,
 )
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.ChaosToken
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.Xp
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid (Pos (..))
import Arkham.Matcher
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheWesternWall.Helpers

newtype TheWesternWall = TheWesternWall ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theWesternWall :: Difficulty -> TheWesternWall
theWesternWall difficulty = scenario TheWesternWall "11517" "The Western Wall" difficulty []

instance HasChaosTokenValue TheWesternWall where
  getChaosTokenValue iid chaosTokenFace (TheWesternWall attrs) = case chaosTokenFace of
    Skull -> do
      lvl <- getLocationLevel iid
      pure $ ChaosTokenValue Skull (NegativeModifier $ byDifficulty attrs lvl (lvl + 2))
    Cultist -> pure $ toChaosTokenValue attrs Cultist 3 5
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 5
    ElderThing -> do
      fullyFlooded <- selectAny $ FullyFloodedLocation <> locationWithInvestigator iid
      pure
        $ if fullyFlooded
          then toChaosTokenValue attrs ElderThing 5 5
          else toChaosTokenValue attrs ElderThing 3 5
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheWesternWall where
  runMessage msg s@(TheWesternWall attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      setChaosTokens (chaosBagContents attrs.difficulty)
      pure s
    Setup -> runScenarioSetup TheWesternWall attrs do
      gather Set.TheWesternWall
      gather Set.CosmicLegacy
      gather Set.DeepOnes
      gather Set.Flood
      gather Set.StarSpawn
      gather Set.TheInescapable
      gather Set.UnderseaCreatures
      gather Set.AgentsOfCthulhu
      -- R'lyeh-set cards gathered via Set.Rlyeh would collide with the trait; the
      -- scenario sets are referenced positionally so they resolve fine here.

      headedWest <- getHasRecord TheExpeditionHeadedWest
      setActDeck [if headedWest then Acts.descendIntoTheAbyss else Acts.ascendTheWall]
      setAgendaDeck [Agendas.floodedPaths]

      -- Western Wall sits at level 1 (grid row 0). The descending Walkway/seafloor
      -- locations occupy levels 2-5 below it.
      westernWall <- placeInGrid (Pos 0 0) Locations.westernWall_11530
      -- TODO: gather the Walkway locations, remove two at random, shuffle them with
      -- Drowned Shanty / Sunken Stairway / Shattered Ruins and place six on levels
      -- 2-4 (unrevealed), then place two more with Obsidian Foundations on level 5.
      obsidianFoundations <- placeInGrid (Pos 4 0) Locations.obsidianFoundations
      startAt (if headedWest then westernWall else obsidianFoundations)
    ScenarioResolution res -> scope "resolutions" do
      -- TODO: cross out "The Western Wall" on the R'lyeh map (needs an R'lyeh-map
      -- campaign-log key/recordable to track which scenarios are completed).
      headedWest <- getHasRecord TheExpeditionHeadedWest
      case res of
        Resolution 1 -> resolutionWithXp "resolution1" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        Resolution 2 -> resolutionWithXp "resolution2" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        NoResolution -> resolutionWithXp "noResolution" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        _ -> error $ "Unknown resolution: " <> show res
      -- TODO: the Do No Harm task / pilgrim story + Hunting Parasite weakness, and
      -- the R'lyeh-map exploration choice. West continues the west path; east
      -- proceeds to Sepulchre of the Sleeper.
      if headedWest
        then setNextCampaignStep TheDrownedQuarter
        else setNextCampaignStep SepulchreOfTheSleeper
      endOfScenario
      pure s
    _ -> TheWesternWall <$> liftRunMessage msg attrs
