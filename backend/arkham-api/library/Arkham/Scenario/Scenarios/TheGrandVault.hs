module Arkham.Scenario.Scenarios.TheGrandVault (theGrandVault) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Import.Lifted (setNextCampaignStep)
import Arkham.Campaigns.TheDrownedCity.CampaignSteps (pattern CourtOfTheAncients, pattern TheApiary)
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Campaigns.TheDrownedCity.Key qualified as Key
import Arkham.Card
import Arkham.ChaosToken
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.Xp
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid (Pos (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheGrandVault.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

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
    PreScenarioSetup -> scope "intro" do
      headedWest <- getHasRecord TheExpeditionHeadedWest
      storyWithContinue' do
        setTitle "title"
        p.basic "checkCampaignLog"
        ul do
          li.validate headedWest "headedWest"
          li.validate (not headedWest) "headedEast"

      withToeTheLine <-
        filterM (`investigatorHasTask` Assets.toeTheLine) =<< select (IncludeEliminated Anyone)
      flavor do
        setTitle "title"
        p $ if headedWest then "grandVault1" else "grandVault2"
        p "grandVault3"
        ul do
          unscoped $ withVars ["token" .= String "elderThing"] $ li "addToken"
          li.validate (notNull withToeTheLine) "resolveToeTheLine"
        p.basic "proceedToSetup"

      -- The campaign handles AddChaosToken by adding to its own bag, so this
      -- sticks for the remainder of the campaign and not just this scenario.
      addChaosToken ElderThing

      for_ withToeTheLine \iid ->
        storyWithChooseOneM'
          ( compose.green do
              h3 "toeTheLine.title"
              p "toeTheLine.instructions"
              p "toeTheLine.body"
              p "toeTheLine.reflection"
              p.basic "toeTheLine.choose"
              ul do
                li "toeTheLine.oldJob"
                li "toeTheLine.highRoad"
          )
          do
            labeled' "toeTheLine.oldJob" do
              decrementRecordCountForInvestigator iid Key.ToeTheLine 1
              nextScenarioFirstAgendaModifier attrs.id attrs iid (AnySkillValue 1)
            labeled' "toeTheLine.highRoad" do
              incrementRecordCountForInvestigator iid Key.ToeTheLine 2
              sufferMentalTrauma iid 1
              nextScenarioFirstAgendaModifier attrs.id attrs iid (AnySkillValue (-1))
      pure s
    StandaloneSetup -> do
      setChaosTokens (chaosBagContents attrs.difficulty)
      pure s
    Setup -> runScenarioSetup TheGrandVault attrs do
      setUsesGrid
      powerWasDiverted <- getHasRecord ThePowerWasDiverted
      scope "setup" $ flavor do
        setTitle "title"
        ul do
          li "gatherSets"
          li.nested "placeFixedLocations" do
            li "removeOtherworldlyMechanisms"
            li "placeVaultChambers"
            li "beginAtTheGreatStair"
          li.nested "checkCampaignLog" do
            li.validate powerWasDiverted "powerWasDiverted"
            li.validate (not powerWasDiverted) "otherwise"
          li "starSpawn"
          li "setCardsAside"
          li "chooseExpeditionAsset"
          li.nested "addFloodTokens" $ li "floodBottomRow"
          li "buildEncounterDeck"
          li "readyToBegin"

      gather Set.TheGrandVault
      gather Set.AlienMachinery
      gather Set.Flood
      gather Set.TheInescapable
      gather Set.Rlyeh
      gather Set.StarSpawn

      setActDeck [Acts.carefulNavigation, Acts.backThroughTheMachine]
      setAgendaDeck [Agendas.bowelsOfTheCity, Agendas.devilInTheMachine, Agendas.everShiftingWalls]

      -- Consume every gathered scenario location; the layout below creates exactly
      -- the copies that remain in this game.
      removeCards =<< amongGathered (CardFromEncounterSet Set.TheGrandVault <> #location)

      -- "Remove one copy of Otherworldly Mechanisms from the game at random", then
      -- shuffle the remaining seven Vault Chambers together.
      mechanisms <-
        pickN
          3
          [ Locations.otherworldlyMechanismsObsidianBulwark
          , Locations.otherworldlyMechanismsSluiceControl
          , Locations.otherworldlyMechanismsGrimeCoveredGears
          , Locations.otherworldlyMechanismsInscrutableApparatus
          ]
      vaultChambers <-
        shuffleM
          $ [ Locations.shroudedCistern
            , Locations.shroudedCistern
            , Locations.chamberOfRecordsArm
            , Locations.chamberOfRecordsEarth
            ]
          <> mechanisms

      -- The vault is a grid navigated only via the Moving Platform; locations are
      -- connected solely by their connection icons or the Platform, so the layout
      -- below only fixes positions and never draws connections.
      greatStair <- placeInGrid (Pos (-2) 0) Locations.theGreatStair
      placeInGrid_ (Pos (-1) 0) Locations.movingPlatformObservationStation
      placeInGrid_ (Pos 0 0) Locations.coreOfTheVaultHeartOfTheMachine
      placeInGrid_ (Pos 2 0) Locations.chamberOfTheTabletUnsealed
      chambers <- for (zip vaultChamberPositions vaultChambers) (uncurry placeInGrid)
      startAt greatStair

      let chamberAt pos = lookup pos (zip vaultChamberPositions chambers)

      -- "If the power was diverted, place 1 resource on the bottom left Vault
      -- Chamber. Otherwise, place one resource on the bottom left, bottom right, and
      -- top right Vault Chamber locations." Written as raw token placement rather
      -- than 'activateLocation', whose already-activated check would query locations
      -- that setup has only queued, not placed.
      let activated = if powerWasDiverted then [vaultBottomLeft] else vaultPreActivatedPositions
      for_ (mapMaybe chamberAt activated) \lid -> placeTokens attrs lid #resource 1

      -- "Shuffle each enemy from the Star Spawn encounter set and remove 2 at random
      -- from the game. Set the rest aside, out of play."
      starSpawn <- shuffleM =<< amongGathered (CardFromEncounterSet Set.StarSpawn <> #enemy)
      removeCards (take 2 starSpawn)
      setAside (drop 2 starSpawn)

      setAside [Assets.tidalTablet]
      setAside
        =<< amongGathered
          ( mapOneOf
              cardIs
              [ Treacheries.ancientVaultO
              , Treacheries.ancientVaultN
              , Treacheries.ancientVaultP
              , Enemies.vaultAttendant
              ]
          )
      setAside =<< amongGathered (CardFromEncounterSet Set.TheInescapable)

      -- "Increase the flood level of the three Vault Chamber locations in the
      -- bottom row."
      for_ (mapMaybe chamberAt vaultBottomRow) increaseFloodLevel

      eachInvestigator (`forInvestigator` Setup)
    ForInvestigator iid Setup -> do
      -- Each Artifact is unique, so one already taken this setup is off the table.
      artifacts <- filterM (fmap not . selectAny . assetIs) =<< getEarnedArtifacts
      chooseOneM iid do
        questionLabeled' "chooseExpeditionAssetQuestion"
        labeled' "noExpeditionAsset" nothing
        for_ (artifacts <> expeditionItems) \asset ->
          cardLabeled asset.cardCode $ handleTarget iid attrs (CardCodeTarget asset.cardCode)
      pure s
    HandleTargetChoice iid (isSource attrs -> True) (CardCodeTarget cardCode) -> do
      for_ (lookupCardDef cardCode) \def -> do
        card <- EncounterCard <$> genEncounterCard def
        createAssetAt_ card (InPlayArea iid)
      pure s
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
