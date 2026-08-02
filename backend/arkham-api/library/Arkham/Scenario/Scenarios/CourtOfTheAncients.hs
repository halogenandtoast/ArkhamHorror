module Arkham.Scenario.Scenarios.CourtOfTheAncients (courtOfTheAncients) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheDrownedCity.CampaignSteps (
  pattern ObsidianCanyons,
  pattern TheGrandVault,
 )
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Campaigns.TheDrownedCity.Key qualified as Key
import Arkham.Card
import Arkham.ChaosToken
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (getSkillTestAction)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid (Pos (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.CourtOfTheAncients.Helpers
import Arkham.Trait (Trait (Stowaway))

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
    Cultist -> do
      stowawayAtYourLocation <-
        selectAny $ EnemyWithTrait Stowaway <> enemyAtLocationWith iid
      pure
        $ if stowawayAtYourLocation
          then toChaosTokenValue attrs Cultist 4 5
          else toChaosTokenValue attrs Cultist 2 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 5
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage CourtOfTheAncients where
  runMessage msg s@(CourtOfTheAncients attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      headedWest <- getHasRecord TheExpeditionHeadedWest
      storyWithContinue' do
        setTitle "title"
        p.basic "checkCampaignLog"
        ul do
          li.validate headedWest "headedWest"
          li.validate (not headedWest) "headedEast"

      investigators <- select (IncludeEliminated Anyone)
      withPlumbTheDepths <- filterM (`investigatorHasTask` Assets.plumbTheDepths) investigators
      flavor do
        setTitle "title"
        if headedWest
          then p "courtOfTheAncients1"
          else do
            p "courtOfTheAncients2"
            p "courtOfTheAncients2Ruby"
            p "courtOfTheAncients2Conclusion"
        ul do
          unscoped
            $ withVars ["token" .= String (if headedWest then "cultist" else "tablet")]
            $ li "addToken"
          li.validate (notNull withPlumbTheDepths) "resolvePlumbTheDepths"
        p.basic "proceedToSetup"

      -- The campaign handles AddChaosToken by adding to its own bag, so this
      -- sticks for the remainder of the campaign and not just this scenario.
      addChaosToken (if headedWest then Cultist else Tablet)

      for_ withPlumbTheDepths \iid ->
        storyWithChooseOneM'
          ( compose.green do
              h3 "plumbTheDepths.title"
              p "plumbTheDepths.instructions"
              p "plumbTheDepths.body"
              p "plumbTheDepths.memories"
              p "plumbTheDepths.reflection"
              p.basic "plumbTheDepths.choose"
              ul do
                li "plumbTheDepths.lookAway"
                li "plumbTheDepths.seekTheTruth"
          )
          do
            -- Both outcomes reach past this scenario: "the next scenario" in a Task
            -- story is the one *after* the scenario the story is read in, and they
            -- affect every investigator, not just the one with the Task.
            labeled' "plumbTheDepths.lookAway" do
              decrementRecordCountForInvestigator iid Key.PlumbTheDepths 1
              for_ investigators \iid' -> nextSetupModifier attrs.id attrs iid' (StartingClues 1)
            labeled' "plumbTheDepths.seekTheTruth" do
              incrementRecordCountForInvestigator iid Key.PlumbTheDepths 2
              sufferMentalTrauma iid 1
              for_ investigators \iid' -> nextSetupModifier attrs.id attrs iid' (StartingHand (-1))
      pure s
    StandaloneSetup -> do
      setChaosTokens (chaosBagContents attrs.difficulty)
      pure s
    Setup -> runScenarioSetup CourtOfTheAncients attrs do
      setUsesGrid
      headedWest <- getHasRecord TheExpeditionHeadedWest
      creatureWasDefeated <- getHasRecord TheCreatureWasDefeated
      scope "setup" $ flavor do
        setTitle "title"
        ul do
          li "gatherSets"
          li.nested "placeFixedLocations" $ li "placeCrumblingArchives"
          li.nested.validate headedWest "checkCampaignLogWest" do
            li "westGreatLift"
            li "westGatherStowaways"
            li "westBuildActAgendaDeck"
            li "westStarSpawn"
            li "westBeginAtEastAntechamber"
          li.nested.validate (not headedWest) "checkCampaignLogEast" do
            li "eastGreatLift"
            li "eastGatherPilgrims"
            li "eastBuildActAgendaDeck"
            li.nested "eastAddFloodTokens" do
              li "eastFloodLevelsOneAndTwo"
              li "eastFullyFloodLevelOne"
            li "eastStarSpawn"
            li "eastBeginAtTwistingCatwalks"
          li "setCardsAside"
          li.nested "checkCampaignLogInescapable" do
            li.validate creatureWasDefeated "creatureWasDefeated"
            li.validate (not creatureWasDefeated) "otherwise"
          li "chooseExpeditionAsset"
          li "buildEncounterDeck"
          li "readyToBegin"

      additionalRules "greatLift"

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

      -- Claimed before the location purge below, which would otherwise sweep the
      -- Great Lift's (Active) side out of the gathered pool.
      setAside [Locations.greatLiftActive, Assets.shardOfYchlecht, Enemies.colossalTyrant]

      -- Consume every gathered scenario location; the layout below creates exactly
      -- the copies that remain in this game. This is also what "remove one copy of
      -- Crumbling Archives from the game at random" amounts to: the sixth archive
      -- is simply never placed.
      removeCards =<< amongGathered (CardFromEncounterSet Set.CourtOfTheAncients <> #location)

      archives <-
        shuffleM
          [ Locations.ringLibraryArchiveOfTheStars
          , Locations.ringLibraryArchiveOfTheAncients
          , Locations.loftyWalkwayArchiveOfDreams
          , Locations.loftyWalkwayArchiveOfConflict
          , Locations.luminousArchivesArchiveOfHistory
          , Locations.luminousArchivesArchiveOfMemory
          ]

      -- The tower is a vertical grid of 4 levels; level = grid row + 1, so level 1
      -- is row 0 and the tower grows upward (matching @GridUp@). Column 1 is the
      -- Great Lift shaft, with the west side in column 0 and the east side in
      -- column 2; Ancient Altar hangs off East Antechamber in column 3, outside
      -- the ring. The five surviving Crumbling Archives fill the ring slots the
      -- four fixed locations leave open.
      let atLevel column level = Pos column (level - 1)
      westAntechamber <- placeInGrid (atLevel 0 1) Locations.westAntechamber
      eastAntechamber <- placeInGrid (atLevel 2 1) Locations.eastAntechamber
      ancientAltar <- placeInGrid (atLevel 3 1) Locations.ancientAltar
      twistingCatwalks <- placeInGrid (atLevel 0 4) Locations.twistingCatwalks
      archiveIds <-
        for (zip [atLevel 2 4, atLevel 0 3, atLevel 2 3, atLevel 0 2, atLevel 2 2] archives)
          $ uncurry placeInGrid

      -- "Put the Great Lift location into play on level 1 [west] / level 4 [east],
      -- (Inactive) side faceup."
      placeInGrid_ (atLevel 1 (if headedWest then 1 else 4)) Locations.greatLiftInactive

      startAt $ if headedWest then eastAntechamber else twistingCatwalks

      -- "Search the Star Spawn encounter set for the Star Spawn Observer enemy and
      -- set it aside, out of play." The west then removes two of the others at
      -- random; the east removes all of them.
      setAside [Enemies.starSpawnObserver]
      otherStarSpawn <- shuffleM =<< amongGathered (CardFromEncounterSet Set.StarSpawn)
      removeCards $ if headedWest then take 2 otherStarSpawn else otherStarSpawn

      -- Only the enemy is set aside; the Still Behind You treacheries in the set
      -- stay in the encounter deck unless the whole set is removed.
      if creatureWasDefeated
        then removeCards =<< amongGathered (CardFromEncounterSet Set.TheInescapable)
        else setAside [Enemies.theInescapable]

      unless headedWest do
        -- "Increase the flood level of each location on levels 1 and 2", then
        -- "increase the flood level of each location on level 1 so that they are
        -- fully flooded" — level 1 ends up fully flooded, level 2 partially. The
        -- Great Lift is on level 4 on this route, so it is untouched.
        let levelOne = [westAntechamber, eastAntechamber, ancientAltar]
        traverse_ increaseFloodLevel $ levelOne <> drop 3 archiveIds
        traverse_ increaseFloodLevel levelOne

      eachInvestigator (`forInvestigator` Setup)
    ForInvestigator iid Setup -> do
      artifacts <- getAvailableArtifacts
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
    ResolveChaosToken _ Tablet iid -> do
      whenAny (locationWithInvestigator iid <> FloodedLocation) $ drawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ ElderThing iid -> do
      action <- getSkillTestAction
      when (action `elem` [Just Action.Fight, Just Action.Evade]) do
        if isEasyStandard attrs
          then chooseOneM iid do
            damageLabeled iid $ assignDamage iid ElderThing 1
            horrorLabeled iid $ assignHorror iid ElderThing 1
          else assignDamageAndHorror iid ElderThing 1 1
      pure s
    ScenarioResolution res -> scope "resolutions" do
      headedWest <- getHasRecord TheExpeditionHeadedWest
      -- Shared by every resolution: cross Court of the Ancients off the R'lyeh map,
      -- and earn the Shard if an investigator still controlled it when the scenario ended.
      crossOutRecordSetEntries RlyehMap [toJSON RlyehCourtOfTheAncients]
      whenM
        (selectAny $ assetIs Assets.shardOfYchlecht <> AssetControlledBy Anyone)
        (record ShardOfYchlecht)
      case res of
        Resolution 1 -> do
          resolutionWithXp "resolution1" $ allGainXp' attrs
          endOfScenarioThen ObsidianCanyons
        Resolution 2 -> do
          resolutionWithXp "resolution2" $ allGainXp' attrs
          endOfScenarioThen TheGrandVault
        NoResolution -> do
          resolutionWithXp "noResolution" $ allGainXp' attrs
          endOfScenarioThen $ if headedWest then ObsidianCanyons else TheGrandVault
        _ -> error $ "Unknown resolution: " <> show res
      pure s
    _ -> CourtOfTheAncients <$> liftRunMessage msg attrs
