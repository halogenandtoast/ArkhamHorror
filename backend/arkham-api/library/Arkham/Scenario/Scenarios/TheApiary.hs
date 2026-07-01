module Arkham.Scenario.Scenarios.TheApiary (theApiary) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Import.Lifted (setNextCampaignStep)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Campaigns.TheDrownedCity.CampaignSteps (pattern TheDrownedQuarter, pattern TheGrandVault)
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Asset.Types (Field (AssetDoom))
import Arkham.ChaosToken
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Types (Field (EnemyDoom))
import Arkham.Helpers.Xp
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheApiary.Helpers

newtype TheApiary = TheApiary ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theApiary :: Difficulty -> TheApiary
theApiary difficulty = scenario TheApiary "11553" "The Apiary" difficulty []

instance HasChaosTokenValue TheApiary where
  getChaosTokenValue iid chaosTokenFace (TheApiary attrs) = case chaosTokenFace of
    Skull -> do
      enemyDoom <- selectSum EnemyDoom AnyEnemy
      assetDoom <- selectSum AssetDoom AnyAsset
      let total = enemyDoom + assetDoom
      pure $ ChaosTokenValue Skull (NegativeModifier $ byDifficulty attrs (min 4 (total `div` 2)) (min 8 total))
    Cultist -> pure $ toChaosTokenValue attrs Cultist 1 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheApiary where
  runMessage msg s@(TheApiary attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      setChaosTokens (chaosBagContents attrs.difficulty)
      pure s
    Setup -> runScenarioSetup TheApiary attrs do
      headedWest <- getHasRecord TheExpeditionHeadedWest
      gather Set.TheApiary
      gather Set.CosmicLegacy
      gather Set.ElderMist
      gather Set.TheInescapable
      gather Set.StrikingFear
      if headedWest
        then do
          -- The pilgrims path: Lost Pilgrims act, Mother removed.
          gather Set.Pilgrims
          gather Set.DarkCult
        else do
          -- The hive-mind path: The Hive Mind act, Maria Rivera removed.
          -- TODO: only the Infected Star Spawn enemy is used; remove the rest of the set.
          gather Set.StarSpawn
          gather Set.Stowaways

      setActDeck [Acts.unsettlingSigns, if headedWest then Acts.lostPilgrims else Acts.theHiveMind]
      setAgendaDeck [Agendas.stirringInTheDark, Agendas.loathsomeParasites]

      -- Every Apiary location enters via its own Revelation, so set them all aside.
      -- The story enemies/assets are likewise set aside; the Central Chamber and its
      -- 4-location ring form as exploration uncovers them.
      -- TODO: per-path removal (west removes Mother, east removes Maria Rivera);
      -- set aside Parasitic Transformation weakness; start-of-scenario Artifact/Item pick.
      setAside
        $ [ Locations.fleshyPathsEasternBurrows
          , Locations.fleshyPathsWesternBurrows
          , Locations.growingFields
          , Locations.churningChasm
          , Locations.corruptedVault
          , Locations.luminousTunnels
          , Locations.spawningGrounds
          , Locations.lostCampsite
          , Locations.graspingCorridor
          , Locations.starvingCorridor
          , Locations.acidicCoelom
          , Locations.centralChamber
          , Locations.hiddenVault
          , Enemies.grotesqueAmalgam
          , Enemies.mother
          , Enemies.squamousParasite
          , Assets.mariaRivera
          , Assets.ancientRelic
          , Assets.grislyMask
          ]

      apiaryEntrance <- place Locations.apiaryEntranceBeckoningLight
      startAt apiaryEntrance
      setScenarioMeta initApiaryMeta
    -- The Hive Mind act flips a coin each round end and rotates the Central
    -- Chamber; the rotation changes which ring location it "faces" (= connects to).
    ScenarioSpecific "rotateCentralChamber" v -> do
      facing <- getCentralChamberFacing
      let dir = toResultDefault ("clockwise" :: Text) v
      let rotate = if dir == "clockwise" then rotateFacingClockwise else rotateFacingCounterClockwise
      setScenarioMeta (ApiaryMeta (rotate facing))
      pure s
    ScenarioResolution res -> scope "resolutions" do
      headedWest <- getHasRecord TheExpeditionHeadedWest
      -- Grisly "Mask" is earned if any investigator still controls it at the end.
      whenM (selectAny $ assetIs Assets.grislyMask) $ record GrislyMask
      case res of
        Resolution 1 -> do
          record ThePilgrimsWereSaved
          resolutionWithXp "resolution1" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        Resolution 2 -> do
          record ThePilgrimsWereDevoured
          resolutionWithXp "resolution2" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        Resolution 3 -> do
          record TheInvestigatorsExterminatedTheAlienParasites
          resolutionWithXp "resolution3" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        NoResolution -> resolutionWithXp "noResolution" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        _ -> error $ "Unknown resolution: " <> show res
      -- TODO: cross out "The Apiary" on the R'lyeh map (R'lyeh-map recordable).
      if headedWest then setNextCampaignStep TheGrandVault else setNextCampaignStep TheDrownedQuarter
      endOfScenario
      pure s
    _ -> TheApiary <$> liftRunMessage msg attrs
