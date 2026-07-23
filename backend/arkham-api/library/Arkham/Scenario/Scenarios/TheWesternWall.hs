module Arkham.Scenario.Scenarios.TheWesternWall (theWesternWall) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Import.Lifted (setNextCampaignStep)
import Arkham.Campaigns.TheDrownedCity.CampaignSteps (
  pattern SepulchreOfTheSleeper,
  pattern TheDrownedQuarter,
 )
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (getFloodLevelFor)
import Arkham.Card
import Arkham.ChaosToken
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (
  ModifierType (..),
  modifiedWith_,
  modifySelectMapM,
  setActiveDuringSetup,
 )
import Arkham.Helpers.Xp
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
    positions <- traverse (field LocationPosition) locations
    let usesNegativeRows = any (maybe False ((< 0) . (.row))) positions
    modifySelectMapM attrs Anywhere \lid -> do
      connections <- runDefaultMaybeT [] do
        pos <- MaybeT $ field LocationPosition lid
        lift $ filterM (isAdjacentLevel pos) locations
      gridOffset <- fieldMap LocationPosition (>>= locationGridOffset usesNegativeRows) lid
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
    locationGridOffset usesNegativeRows pos
      | usesNegativeRows && pos.row == -2 = Just (0.5, 0)
      | usesNegativeRows = Nothing
      | otherwise = Just (if pos.row == 2 then 0.5 else 0, fromIntegral (2 * pos.row - 4))

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

      hasDoNoHarm <- selectAny $ InDeckOf Anyone <> basic (cardIs Assets.doNoHarm)
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
      when headedWest do
        scope "setupV1" $ flavor do
          setTitle "title"
          ul do
            li "gatherSets"
            li.nested "gatherLocations" do
              li "placeWesternWall"
              li "setLocationsAside"
              li "removeWalkways"
              li "placeUpperWalkways"
              li "placeBottomWalkways"
              li "beginAtWesternWall"
            li "setCardsAside"
            li "setAsideCoralStarSpawn"
            li "buildActDeck"
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
      bottomRow <- shuffleM $ Locations.obsidianFoundations : bottomWalkways

      westernWall <- placeInGrid (Pos 1 0) Locations.westernWall_11530
      upperLocations <-
        for
          (zip [Pos 1 (-1), Pos 0 (-2), Pos 1 (-2), Pos 0 (-3), Pos 1 (-3), Pos 2 (-3)] upperWalkways)
          $ uncurry placeInGrid
      bottomLocations <-
        for (zip [Pos 0 (-4), Pos 1 (-4), Pos 2 (-4)] bottomRow) $ uncurry placeInGrid
      let obsidianFoundations =
            snd
              $ fromJustNote "Missing Obsidian Foundations"
              $ find ((== Locations.obsidianFoundations) . fst)
              $ zip bottomRow bottomLocations
          startingLocation = if headedWest then westernWall else obsidianFoundations
          levelFourAndFive = drop 3 upperLocations <> bottomLocations

      traverse_ (push . IncreaseFloodLevel) $ upperLocations <> bottomLocations
      traverse_ (push . IncreaseFloodLevel) levelFourAndFive
      startAt startingLocation
      whenHasRecord TheExpeditionLeftThePilgrim $ removeAllClues attrs startingLocation
      eachInvestigator (`forInvestigator` Setup)
    ForInvestigator iid Setup -> do
      chooseOneM iid do
        questionLabeled' "setupV1.chooseExpeditionAssetQuestion"
        labeled' "setupV1.noExpeditionAsset" nothing
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
    ScenarioResolution res -> scope "resolutions" do
      -- TODO: cross out "The Western Wall" on the R'lyeh map (needs an R'lyeh-map
      -- campaign-log key/recordable to track which scenarios are completed).
      headedWest <- getHasRecord TheExpeditionHeadedWest
      case res of
        Resolution 1 -> resolutionWithXp "resolution1" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        Resolution 2 -> resolutionWithXp "resolution2" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        NoResolution -> resolutionWithXp "noResolution" $ allGainXpWithBonus' attrs $ toBonus "bonus" 0
        _ -> error $ "Unknown resolution: " <> show res
      -- TODO: the Hunting Parasite weakness and the R'lyeh-map exploration choice.
      -- West continues the west path; east
      -- proceeds to Sepulchre of the Sleeper.
      setNextCampaignStep
        $ if headedWest
          then TheDrownedQuarter
          else SepulchreOfTheSleeper
      endOfScenario
      pure s
    _ -> TheWesternWall <$> liftRunMessage msg attrs
