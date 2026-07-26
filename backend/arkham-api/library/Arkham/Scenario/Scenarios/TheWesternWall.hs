module Arkham.Scenario.Scenarios.TheWesternWall (theWesternWall) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheDrownedCity.CampaignSteps (
  pattern SepulchreOfTheSleeper,
  pattern TheApiary,
  pattern TheDrownedQuarter,
 )
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Campaigns.TheDrownedCity.Key qualified as Key
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (getFloodLevelFor)
import Arkham.Card
import Arkham.ChaosToken
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (
  ModifierType (..),
  modifiedWith_,
  modifySelectMapM,
  setActiveDuringSetup,
 )
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.FloodLevel (FloodLevel (FullyFlooded))
import Arkham.Location.Grid (Pos (..))
import Arkham.Location.Types (Field (LocationPosition))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Modifier (UIModifier (..))
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheWesternWall.Helpers
import Arkham.SortedPair
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheWesternWall = TheWesternWall ScenarioAttrs
  deriving stock Generic
  deriving anyclass IsScenario
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

instance HasModifiersFor TheWesternWall where
  getModifiersFor (TheWesternWall attrs) = do
    locations <- select Anywhere
    underseaVaultPosition <-
      listToMaybe . catMaybes <$> selectField LocationPosition (locationIs Locations.underseaVault)
    modifySelectMapM attrs Anywhere \lid -> do
      connections <- runDefaultMaybeT [] do
        pos <- MaybeT $ field LocationPosition lid
        lift $ filterM (isAdjacentLevel pos) locations
      gridOffset <- fieldMap LocationPosition (>>= locationGridOffset underseaVaultPosition) lid
      for_ gridOffset \(columnOffset, rowOffset) ->
        modifiedWith_ attrs lid setActiveDuringSetup [UIModifier (GridOffset columnOffset rowOffset)]
      for_ connections \connected ->
        modifiedWith_ attrs lid setActiveDuringSetup [DoNotDrawConnection $ sortedPair lid connected]
      pure
        [ ConnectedToWhen (LocationWithId lid) (mapOneOf LocationWithId connections)
        | notNull connections
        ]
   where
    isAdjacentLevel pos other =
      fieldMap LocationPosition (maybe False ((== 1) . abs . subtract pos.row . (.row))) other
    -- Stagger level 3 without changing the grid's vertical orientation. Positive
    -- rows naturally render above row 0 and negative rows naturally render below it.
    locationGridOffset underseaVaultPosition pos
      | Just vaultPos <- underseaVaultPosition
      , pos.row == vaultPos.row
      , abs pos.row == 2 =
          Nothing
      | Just vaultPos <- underseaVaultPosition
      , pos.row == vaultPos.row
      , abs pos.row == 4 =
          Just (-0.5, 0)
      | abs pos.row == 2 = Just (0.5, 0)
      | otherwise = Nothing

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
      floodLevel <- getFloodLevelFor iid
      pure
        $ if floodLevel == FullyFlooded
          then toChaosTokenValue attrs ElderThing 5 5
          else toChaosTokenValue attrs ElderThing 3 5
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheWesternWall where
  runMessage msg s@(TheWesternWall attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      headedWest <- getHasRecord TheExpeditionHeadedWest
      storyWithContinue' do
        setTitle "title"
        p.basic "checkCampaignLog"
        ul do
          li.validate headedWest "headedWest"
          li.validate (not headedWest) "headedEast"

      hasDoNoHarm <-
        anyM (`investigatorHasTask` Assets.doNoHarm)
          =<< select (IncludeEliminated Anyone)
      flavor do
        setTitle "title"
        p $ if headedWest then "westernWall1" else "westernWall2"
        ul $ li.validate hasDoNoHarm "resolveDoNoHarm"
        p.basic $ if headedWest then "proceedToWesternSetup" else "proceedToEasternSetup"

      when hasDoNoHarm do
        storyWithChooseOneM'
          ( compose.green do
              h3 "doNoHarm.title"
              p "doNoHarm.instructions"
              p "doNoHarm.body"
              p.basic "doNoHarm.choose"
              ul do
                li "doNoHarm.bringAlong"
                li "doNoHarm.leaveBehind"
          )
          do
            labeled' "doNoHarm.bringAlong" $ record TheExpeditionHelpedThePilgrim
            labeled' "doNoHarm.leaveBehind" $ record TheExpeditionLeftThePilgrim
      pure s
    StandaloneSetup -> do
      setChaosTokens (chaosBagContents attrs.difficulty)
      pure s
    Setup -> runScenarioSetup TheWesternWall attrs do
      headedWest <- getHasRecord TheExpeditionHeadedWest
      -- Bind the scope name rather than a partially applied `scope`, which the
      -- monomorphism restriction would pin to a single builder type.
      let version = if headedWest then "west" else "east" :: Text
      scope "setup" $ flavor do
        scope version $ setTitle "title"
        ul do
          li "gatherSets"
          li.nested "gatherLocations" do
            li $ if headedWest then "placeWesternWall" else "placeObsidianFoundations"
            scope version $ li "setLocationsAside"
            li "removeWalkways"
            scope version $ li "placeUpperWalkways"
            scope version $ li "placeBottomWalkways"
            li $ if headedWest then "beginAtWesternWall" else "beginAtObsidianFoundations"
          li "setCardsAside"
          li "setAsideCoralStarSpawn"
          scope version $ li "buildActDeck"
          li "chooseExpeditionAsset"
          li.nested "addFloodTokens" do
            li "floodLevelsTwoToFive"
            li "fullyFloodLevelsFourAndFive"
          li "buildEncounterDeck"
          li "readyToBegin"

      additionalRules "locationAdjacency"

      gather Set.TheWesternWall
      gather Set.CosmicLegacy
      gather Set.DeepOnes
      gather Set.Flood
      gather Set.Rlyeh
      gather Set.StarSpawn
      gather Set.TheInescapable
      gather Set.UnderseaCreatures
      gather Set.AgentsOfCthulhu

      setActDeck [if headedWest then Acts.descendIntoTheAbyss else Acts.ascendTheWall]
      setAgendaDeck [Agendas.floodedPaths]
      whenHasRecord TheExpeditionHelpedThePilgrim $ placeDoomOnAgenda 1

      setAside
        [ Enemies.theInescapable
        , Enemies.huntingParasite
        , Treacheries.seafloorFrieze
        , Locations.underseaVault
        ]
      setAside =<< amongGathered (cardIs Enemies.coralStarSpawn)
      removeCards =<< amongGathered (CardFromEncounterSet Set.StarSpawn)

      -- Consume all scenario locations from the gathered cards; the selected
      -- layout below creates exactly the copies that remain in this game.
      removeCards =<< amongGathered (CardFromEncounterSet Set.TheWesternWall <> #location)
      shuffledPaths <-
        shuffleM
          [ Locations.treacherousPathSlickSteps
          , Locations.treacherousPathSlickSteps
          , Locations.treacherousPathErodedShelf
          , Locations.treacherousPathErodedShelf
          , Locations.treacherousPathPrecariousClimb
          , Locations.treacherousPathDeadlyPass
          , Locations.treacherousPathShallowDen
          ]
      let remainingPaths = drop 2 shuffledPaths
      mixedWalkways <-
        shuffleM
          $ remainingPaths
          <> [Locations.drownedShanty, Locations.sunkenStairway, Locations.shatteredRuins]
      let (upperWalkways, bottomWalkways) = splitAt 6 mixedWalkways
      bottomRow <-
        shuffleM
          $ (if headedWest then Locations.obsidianFoundations else Locations.westernWall_11530)
          : bottomWalkways

      -- V.I descends from Western Wall, so increasing levels use negative rows.
      -- V.II ascends from Western Wall, so increasing levels use positive rows.
      -- Keeping Western Wall at row 0 makes abs(row) + 1 the level in both layouts.
      let rowForLevel level = (if headedWest then negate else id) (level - 1)
          atLevel column level = Pos column (rowForLevel level)
      startingLocation <-
        placeInGrid (atLevel 1 1)
          $ if headedWest then Locations.westernWall_11530 else Locations.obsidianFoundations
      upperLocations <-
        for
          ( zip
              [ atLevel 1 2
              , atLevel 0 3
              , atLevel 1 3
              , atLevel 0 4
              , atLevel 1 4
              , atLevel 2 4
              ]
              upperWalkways
          )
          $ uncurry placeInGrid
      bottomLocations <-
        for (zip [atLevel 0 5, atLevel 1 5, atLevel 2 5] bottomRow) $ uncurry placeInGrid
      let levelFourAndFive = drop 3 upperLocations <> bottomLocations

      traverse_ (push . IncreaseFloodLevel) $ upperLocations <> bottomLocations
      traverse_ (push . IncreaseFloodLevel) levelFourAndFive
      startAt startingLocation
      whenHasRecord TheExpeditionLeftThePilgrim $ removeAllClues attrs startingLocation
      eachInvestigator (`forInvestigator` Setup)
    ForInvestigator iid Setup -> do
      chooseOneM iid do
        questionLabeled' "chooseExpeditionAssetQuestion"
        labeled' "noExpeditionAsset" nothing
        for_
          [ Assets.expeditionGear
          , Assets.laudanum
          , Assets.alienTablet
          , Assets.divingSuitTheDrownedCity
          ]
          \asset -> cardLabeled asset.cardCode $ handleTarget iid attrs (CardCodeTarget asset.cardCode)
      pure s
    HandleTargetChoice iid (isSource attrs -> True) (CardCodeTarget cardCode) -> do
      for_ (lookupCardDef cardCode) \def -> do
        card <- EncounterCard <$> genEncounterCard def
        createAssetAt_ card (InPlayArea iid)
      pure s
    ResolveChaosToken _ Cultist iid | isHardExpert attrs -> do
      whenM ((== FullyFlooded) <$> getFloodLevelFor iid) $ assignDamage iid Cultist 1
      pure s
    ResolveChaosToken _ Tablet iid | isHardExpert attrs -> do
      withLocationOf iid $ push . IncreaseFloodLevel
      pure s
    FailedSkillTestWithToken iid Cultist | isEasyStandard attrs -> do
      whenM ((== FullyFlooded) <$> getFloodLevelFor iid) $ assignDamage iid Cultist 1
      pure s
    FailedSkillTestWithToken iid Tablet | isEasyStandard attrs -> do
      withLocationOf iid $ push . IncreaseFloodLevel
      pure s
    ScenarioResolution res -> scope "resolutions" do
      crossOutRecordSetEntries RlyehMap [toJSON RlyehWesternWall]
      headedWest <- getHasRecord TheExpeditionHeadedWest
      let resolveDoNoHarm = eachInvestigator \iid -> do
            whenM (investigatorHasTask iid Assets.doNoHarm) do
              helpedThePilgrim <- getHasRecord TheExpeditionHelpedThePilgrim
              scope "doNoHarmResolution" $ flavor $ compose.green do
                h3 "title"
                p "instructions"
                p.basic "checkCampaignLog"
                ul do
                  li.validate helpedThePilgrim "helpedThePilgrim"
                  li.validate (not helpedThePilgrim) "otherwise"
                if helpedThePilgrim
                  then do
                    p "task1"
                    ul do
                      li "task1HuntingParasite"
                      li "task1Progress"
                  else do
                    p "task2"
                    ul do
                      li "task2Trauma"
                      li "task2Progress"
              if helpedThePilgrim
                then do
                  addCampaignCardToDeck iid DoNotShuffleIn Enemies.huntingParasite
                  incrementRecordCountForInvestigator iid Key.DoNoHarm 2
                else sufferMentalTrauma iid 1
          chooseResolution3 =
            storyWithChooseOneM'
              (compose.resolution $ scope "resolution3" $ setTitle "title" >> p "body")
              do
                labeled' "resolution3.drownedQuarter" $ endOfScenarioThen TheDrownedQuarter
                labeled' "resolution3.apiary" $ endOfScenarioThen TheApiary
      case res of
        Resolution 1 -> do
          resolutionWithXp "resolution1" $ allGainXp' attrs
          resolveDoNoHarm
          chooseResolution3
        Resolution 2 -> do
          resolutionWithXp "resolution2" $ allGainXp' attrs
          resolveDoNoHarm
          endOfScenarioThen SepulchreOfTheSleeper
        NoResolution -> do
          resolutionWithXp "noResolution" $ allGainXp' attrs
          resolveDoNoHarm
          if headedWest then chooseResolution3 else endOfScenarioThen SepulchreOfTheSleeper
        _ -> error $ "Unknown resolution: " <> show res
      pure s
    _ -> TheWesternWall <$> liftRunMessage msg attrs
