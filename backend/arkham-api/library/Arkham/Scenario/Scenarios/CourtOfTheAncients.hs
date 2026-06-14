module Arkham.Scenario.Scenarios.CourtOfTheAncients (courtOfTheAncients) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Import.Lifted (setNextCampaignStep)
import Arkham.Campaigns.TheDrownedCity.CampaignSteps (pattern ObsidianCanyons, pattern TheGrandVault)
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.ChaosToken
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Grid (Pos (..))
import Arkham.Helpers.Xp
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.CourtOfTheAncients.Helpers

newtype CourtOfTheAncients = CourtOfTheAncients ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

courtOfTheAncients :: Difficulty -> CourtOfTheAncients
courtOfTheAncients difficulty = scenario CourtOfTheAncients "11612" "Court of the Ancients" difficulty []

instance HasChaosTokenValue CourtOfTheAncients where
  getChaosTokenValue iid chaosTokenFace (CourtOfTheAncients attrs) = case chaosTokenFace of
    Skull -> do
      glyphs <- getVictoryGlyphCount
      pure $ ChaosTokenValue Skull (NegativeModifier $ byDifficulty attrs glyphs (glyphs + 1))
    -- TODO: cultist -2/-3 becomes -4/-5 with a Stowaway enemy at your location.
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 5
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage CourtOfTheAncients where
  runMessage msg s@(CourtOfTheAncients attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      setChaosTokens (chaosBagContents attrs.difficulty)
      pure s
    Setup -> runScenarioSetup CourtOfTheAncients attrs do
      headedWest <- getHasRecord TheExpeditionHeadedWest
      gather Set.CourtOfTheAncients
      gather Set.Domination
      gather Set.Dreams
      gather Set.ElderMist
      gather Set.TheInescapable
      gather Set.Rlyeh
      gather Set.StarSpawn
      if headedWest then gather Set.Stowaways else gather Set.Pilgrims

      setActDeck [Acts.stepsOfGiants, if headedWest then Acts.escapeTheTowerV1 else Acts.escapeTheTowerV2]
      setAgendaDeck
        [ if headedWest then Agendas.ruinedArchives else Agendas.floodedArchives
        , Agendas.unstableFoundations
        ]

      -- The tower is a vertical grid; level = grid row + 1 (so level 1 = row 0).
      -- The Great Lift starts Inactive on level 1 (west) or level 4 (east).
      -- TODO: lay out Twisting Catwalks / West & East Antechamber / Ancient Altar and
      -- the shuffled Crumbling Archives (remove one at random) per the page-27 diagram;
      -- on the east route flood levels 1-2 (level 1 fully); Star Spawn cull (keep only
      -- the Star Spawn Observer); The Inescapable removed if "the creature was defeated".
      let liftRow = if headedWest then 0 else 3
      _greatLift <- placeInGrid (Pos liftRow 1) Locations.greatLiftInactive
      _westAntechamber <- placeInGrid (Pos liftRow 0) Locations.westAntechamber
      eastAntechamber <- placeInGrid (Pos liftRow 2) Locations.eastAntechamber
      twistingCatwalks <- placeInGrid (Pos 3 1) Locations.twistingCatwalks
      _ancientAltar <- place Locations.ancientAltar

      if headedWest then startAt eastAntechamber else startAt twistingCatwalks

      setAside
        [ Locations.greatLiftActive
        , Assets.shardOfYchlecht
        , Enemies.colossalTyrant
        ]
    ScenarioResolution res -> scope "resolutions" do
      headedWest <- getHasRecord TheExpeditionHeadedWest
      -- Shard of Y'ch'lecht is earned if an investigator still controls it at the end.
      whenM (selectAny $ assetIs Assets.shardOfYchlecht) $ record ShardOfYchlecht
      case res of
        Resolution 1 -> resolutionWithXp "resolution1" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        Resolution 2 -> resolutionWithXp "resolution2" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        NoResolution -> resolutionWithXp "noResolution" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        _ -> error $ "Unknown resolution: " <> show res
      -- TODO: cross out "Court of the Ancients" on the R'lyeh map (R'lyeh-map recordable).
      if headedWest then setNextCampaignStep ObsidianCanyons else setNextCampaignStep TheGrandVault
      endOfScenario
      pure s
    _ -> CourtOfTheAncients <$> liftRunMessage msg attrs
