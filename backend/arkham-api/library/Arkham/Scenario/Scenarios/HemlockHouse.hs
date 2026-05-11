module Arkham.Scenario.Scenarios.HemlockHouse (hemlockHouse) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetLocation))
import Arkham.Attack (enemyAttack)
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Campaigns.TheFeastOfHemlockVale.Key
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.EnemyLocation.Cards qualified as EnemyLocations
import Arkham.EnemyLocation.Types (enemyLocationAsEnemyId)
import {-# SOURCE #-} Arkham.Game.Utils (maybeEnemyLocation)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Xp
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (incrementRecordCount, record, remember)
import Arkham.Message.Lifted.Story (resolveStoryWithPlacement)
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (Field (ScenarioVictoryDisplay))
import Arkham.ScenarioLogKey (ScenarioLogKey (LittleSylvieCanBeTakenControl))
import Arkham.Scenarios.HemlockHouse.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Token
import Arkham.Treachery.Cards qualified as Treacheries

newtype HemlockHouse = HemlockHouse ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hemlockHouse :: Difficulty -> HemlockHouse
hemlockHouse difficulty = scenario HemlockHouse "10523" "Hemlock House" difficulty []

instance HasChaosTokenValue HemlockHouse where
  getChaosTokenValue iid tokenFace (HemlockHouse attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage HemlockHouse where
  runMessage msg s@(HemlockHouse attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      day <- getCampaignDay
      time <- getCampaignTime
      let isNight = time == Night
      -- "Check your Campaign Log" routing flavor (PDF page 14, top).
      flavor do
        setTitle "title"
        p.basic "body"
        ul $ li.nested.validate isNight "nightSkip" do
          li.validate (not isNight && day == Day1) "day1"
          li.validate (not isNight && day == Day2) "day2"
          li.validate (not isNight && day == Day3) "day3"
      -- Intro routing per scenario reference:
      --   Day 1 → Intro 1
      --   Day 2 → Intro 2
      --   Day 3 → Intro 3
      --   Night 1/Night 2 → Intro 4
      case (day, time) of
        (Day1, Day) -> story $ i18nWithTitle "intro1"
        (Day2, Day) -> story $ i18nWithTitle "intro2"
        (Day3, Day) -> story $ i18nWithTitle "intro3"
        _ -> story $ i18nWithTitle "intro4"
      pure s
    Setup -> runScenarioSetup HemlockHouse attrs do
      setup $ ul do
        li "gatherSets"
        li "currentDaySet"
        li "currentDayMarker"
        li "againstTheHouse"
        li "houseStirsV2"
        li "houseStirsV1"
        li.nested "locations" do
          li "firstFloor"
          li "shuffleFloors"
          li "topBedroom"
          li "removeRemaining"
          li "startAtFoyer"
          li "startAtBedroom"
        li.nested "residents" do
          li "gideonAndSylvie"
          li "william"
          li "judith"
          li "theo"
          li "removeResidents"
        li "predatoryHouse"
        li "setOutOfPlay"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      setUsesGrid

      gather Set.HemlockHouse
      gather Set.AgentsOfTheColour
      gather Set.Blight
      gather Set.Fire
      gather Set.Transfiguration
      gather Set.LockedDoors
      gather Set.Rats

      gatherAndSetAside Set.Residents

      setScenarioDayAndTime
      day <- getCampaignDay
      time <- getCampaignTime

      let
        agenda2 =
          case day of
            Day2 -> Agendas.theHouseStirsV2
            _ -> Agendas.theHouseStirsV1

      setAgendaDeck [Agendas.eerieSilence, agenda2, Agendas.livingWalls]
      setActDeck [Acts.strangeInfestation, Acts.theHeartOfTheHouse]

      (topBedroom, rest) <-
        sampleWithRest
          $ Locations.bedroomHemlockHouse32
          :| [ Locations.bedroomHemlockHouse33
             , Locations.bedroomHemlockHouse34
             , Locations.bedroomHemlockHouse35
             ]

      locations <-
        sampleN 6
          $ Locations.washroomHemlockHouse36
          :| ( [ Locations.washroomHemlockHouse37
               , Locations.washroomHemlockHouse38
               , Locations.libraryHemlockHouse39
               , Locations.libraryHemlockHouse40
               ]
                 <> rest
             )

      parlor <- placeInGrid (Pos (-1) 0) Locations.parlorHemlockHouse
      foyer <- placeInGrid (Pos 0 0) Locations.foyerHemlockHouse
      placeInGrid_ (Pos 1 0) Locations.diningRoomHemlockHouse

      bedroom <- placeInGrid (Pos 0 3) topBedroom

      -- Track placed locations on floors 2 and 3 so we can locate the
      -- lowest-floor Bedroom for Sylvie's setup placement on Day 1.
      placedFloors23 <-
        for (zip [Pos x y | x <- [-1 .. 1], y <- [1, 2]] locations) \(pos, def) -> do
          lid <- placeInGrid pos def
          pure (def, lid, pos)

      -- "Set the Shapeless Cellar location and one random copy of the Bedroom
      -- location aside, out of play." Shapeless Cellar is an enemy-location
      -- (no Dormant side), so it isn't included in `fromGathered #location`;
      -- pull it from the gathered encounter set directly.
      setAside [EnemyLocations.shapelessCellar]

      void $ fromGathered #location

      case day of
        Day1 -> do
          gather Set.TheFirstDay
          placeStory $ case time of
            Day -> Stories.dayOne
            Night -> Stories.nightOne
        Day2 -> do
          gather Set.TheSecondDay
          placeStory $ case time of
            Day -> Stories.dayTwo
            Night -> Stories.nightTwo
        Day3 -> do
          gather Set.TheFinalDay
          placeStory $ case time of
            Day -> Stories.dayThree
            Night -> Stories.nightThree

      startAt $ if day == Day3 then bedroom else foyer

      setAside [Acts.againstTheHouse]

      -- "Set all five Fire! treacheries and all copies of the Out of the Walls
      -- and Pulled In treacheries aside, out of play."
      setAside $ replicate 5 Treacheries.fire
      setAside $ replicate 4 Treacheries.outOfTheWalls
      setAside $ replicate 2 Treacheries.pulledIn

      -- Pull Little Sylvie out of the encounter deck regardless of day/time —
      -- she has a player card back and should never sit in the encounter deck.
      -- On Day 1 (Day) she enters play below; otherwise she stays set aside.
      setAside [Assets.littleSylvie]

      -- Place residents per day. Per scenario reference: only when it is
      -- (Day) — at Night, no residents enter play during setup. Unplaced
      -- residents remain set aside (via the gatherAndSetAside above) so they
      -- can enter play via codex callbacks.
      when (time == Day) $ case day of
        Day1 -> do
          assetAt_ Assets.gideonMizrahSeasonedSailor parlor
          assetAt_ Assets.williamHemlockAspiringPoet bedroom
          -- "Put the Little Sylvie story asset into play at a Bedroom location
          -- on the lowest floor." Find the Bedroom in the floors 2/3 placement
          -- pool with the smallest y; if none, fall back to the top bedroom.
          let bedroomCodes :: Set CardCode =
                setFromList
                  $ map
                    toCardCode
                    [ Locations.bedroomHemlockHouse32
                    , Locations.bedroomHemlockHouse33
                    , Locations.bedroomHemlockHouse34
                    , Locations.bedroomHemlockHouse35
                    ]
              placedBedrooms =
                [ (lid, y)
                | (def, lid, Pos _ y) <- placedFloors23
                , toCardCode def `member` bedroomCodes
                ]
              lowestBedroom = case sortOn snd placedBedrooms of
                ((lid, _) : _) -> lid
                [] -> bedroom
          assetAt_ Assets.littleSylvie lowestBedroom
        Day2 -> do
          assetAt_ Assets.judithParkTheMuscle parlor
          assetAt_ Assets.williamHemlockAspiringPoet bedroom
        Day3 -> do
          assetAt_ Assets.judithParkTheMuscle parlor
          assetAt_ Assets.theoPetersJackOfAllTrades foyer

      lead <- getLead
      thePredatoryHouse <- genCard Stories.thePredatoryHouse
      resolveStoryWithPlacement lead thePredatoryHouse Global
    ScenarioSpecific "enemyLocationDefeated" (maybeResult -> Just lid) -> do
      grid <- getGrid
      lead <- getLead
      -- InvestigatorAt (LocationWithId lid) returns empty for enemy-locations because
      -- they are in enemyLocationsL, not locationsL.
      investigators <-
        filterM (\iid -> (== Just lid) <$> field InvestigatorLocation iid)
          =<< select UneliminatedInvestigator
      enemies <- select $ EnemyAt (LocationWithId lid)
      storyAssets <- select $ AssetAt (LocationWithId lid) <> StoryAsset
      push $ AddToVictory Nothing (LocationTarget lid)
      case findInGrid lid grid of
        Nothing -> pure s
        Just (Pos col row) -> do
          let locationsAbove =
                [ (y, locLid)
                | y <- [row + 1 .. row + 20]
                , Just (GridLocation _ locLid) <- [viewGrid (Pos col y) grid]
                ]
          if null locationsAbove
            then do
              let adjPositions =
                    [ Pos (col + 1) row
                    , Pos (col - 1) row
                    , Pos col (row + 1)
                    , Pos col (row - 1)
                    ]
                  adjacentLids =
                    [ locLid
                    | adjp <- adjPositions
                    , Just (GridLocation _ locLid) <- [viewGrid adjp grid]
                    ]
                  hasEntities =
                    not (null investigators && null enemies && null storyAssets)
              when (not (null adjacentLids) && hasEntities)
                $ chooseOrRunOne
                  lead
                  [ targetLabel targetLid
                      $ [PlaceInvestigator iid (AtLocation targetLid) | iid <- investigators]
                      <> [EnemyMove eid targetLid | eid <- enemies]
                      <> [PlaceAsset aid (AtLocation targetLid) | aid <- storyAssets]
                  | targetLid <- adjacentLids
                  ]
            else do
              pushAll
                [ PlaceGrid (GridLocation (Pos col (y - 1)) locLid)
                | (y, locLid) <- locationsAbove
                ]
              case locationsAbove of
                (_, targetLid) : _ ->
                  pushAll
                    $ [PlaceInvestigator iid (AtLocation targetLid) | iid <- investigators]
                    <> [EnemyMove eid targetLid | eid <- enemies]
                    <> [PlaceAsset aid (AtLocation targetLid) | aid <- storyAssets]
                [] -> pure ()
          let middleColEmpty =
                col == 0 && null [l | GridLocation (Pos 0 _) l <- flattenGrid grid, l /= lid]
          when middleColEmpty
            $ pushAll
              [ PlaceGrid (GridLocation (Pos 0 y) locLid)
              | GridLocation (Pos (-1) y) locLid <- flattenGrid grid
              ]
          pure s
    ScenarioSpecific "predationTablet" (maybeResult -> Just lead) -> do
      investigators <- select UneliminatedInvestigator
      attackPairs <-
        investigators & mapMaybeM \iid -> runMaybeT do
          lid <- MaybeT $ getLocationOf iid
          el <- MaybeT $ maybeEnemyLocation lid
          pure (enemyLocationAsEnemyId el, iid)
      for_ attackPairs \(eid, iid) ->
        push $ InitiateEnemyAttack $ enemyAttack eid attrs iid
      when (null attackPairs)
        $ drawEncounterCard lead attrs
      pure s
    ScenarioSpecific "codex" v -> scope "codex" do
      let (iid :: InvestigatorId, source :: Source, n :: Int) = toResult v
      let entry x = scope x $ flavor $ setTitle "title" >> p "body"
      day <- getCampaignDay
      case n of
        4 -> do
          entry "william"
          william <- selectJust $ assetIs Assets.williamHemlockAspiringPoet
          case day of
            Day1 -> do
              controlsSylvie <-
                selectAny
                  $ assetIs Assets.littleSylvie
                  <> AssetControlledBy (InvestigatorWithId iid)
              if controlsSylvie
                then do
                  takeControlOfAsset iid william
                  record FoundLittleSylvie
                  incrementRecordCount WilliamHemlockRelationshipLevel 1
                  interludeXpAll (toBonus "bonus" 1)
                else do
                  eachInvestigator \iid' -> gainClues iid' source 1
                  -- "For the rest of the scenario, the Little Sylvie story
                  -- asset gains '[fast]: Take control of Little Sylvie.'"
                  remember LittleSylvieCanBeTakenControl
            Day2 -> do
              judith <- selectOne $ assetIs Assets.judithParkTheMuscle
              williamLoc <- field AssetLocation william
              sameLoc <- case (judith, williamLoc) of
                (Just jid, Just wloc) ->
                  (== Just wloc) <$> field AssetLocation jid
                _ -> pure False
              if sameLoc
                then scenarioSpecific "codex" (iid, source, Sigma)
                else takeControlOfAsset iid william
            Day3 -> takeControlOfAsset iid william
        6 -> do
          entry "gideon"
          helping <- getHasRecord YouAreHelpingGideon
          if helping
            then do
              incrementRecordCount GideonMizrahRelationshipLevel 1
              interludeXpAll (toBonus "bonus" 1)
            else do
              record YouAreHelpingGideon
              predatoryHouse <- selectJust $ storyIs Stories.thePredatoryHouse
              sendMessage predatoryHouse $ ScenarioSpecific "cancelNextPredation" Null
        7 -> do
          entry "judith"
          judith <- selectJust $ assetIs Assets.judithParkTheMuscle
          case day of
            Day2 -> do
              william <- selectOne $ assetIs Assets.williamHemlockAspiringPoet
              judithLoc <- field AssetLocation judith
              sameLoc <- case (william, judithLoc) of
                (Just wid, Just jloc) ->
                  (== Just jloc) <$> field AssetLocation wid
                _ -> pure False
              if sameLoc
                then scenarioSpecific "codex" (iid, source, Sigma)
                else takeControlOfAsset iid judith
            Day3 -> do
              takeControlOfAsset iid judith
              record JudithIsRemodeling
            _ -> takeControlOfAsset iid judith
        8 -> do
          entry "theo"
          theo <- selectJust $ assetIs Assets.theoPetersJackOfAllTrades
          takeControlOfAsset iid theo
        Theta -> do
          entry "marquez"
          drawCards iid source 3
          grid <- getGrid
          enemyLocations <-
            mapMaybeM
              (\lid -> fmap (lid,) <$> maybeEnemyLocation lid)
              [lid | GridLocation _ lid <- flattenGrid grid]
          chooseOneM iid do
            for_ enemyLocations \(lid, el) ->
              targeting lid
                $ nonAttackEnemyDamage (Just iid) source 1 (enemyLocationAsEnemyId el)
        Sigma -> do
          entry "argument1"
          chooseOneM iid do
            labeled' "judithRight" $ scenarioSpecific "codex" (iid, source, SigmaJudithRight)
            labeled' "williamRight" $ scenarioSpecific "codex" (iid, source, SigmaWilliamRight)
        SigmaJudithRight -> do
          entry "argument2"
          william <- selectJust $ assetIs Assets.williamHemlockAspiringPoet
          removeFromGame william
          eachInvestigator \iid' -> drawCards iid' source 1
          incrementRecordCount JudithParkRelationshipLevel 1
          interludeXpAll (toBonus "bonus" 1)
        SigmaWilliamRight -> do
          entry "argument3"
          judith <- selectJust $ assetIs Assets.judithParkTheMuscle
          removeFromGame judith
          eachInvestigator \iid' -> drawCards iid' source 1
          incrementRecordCount WilliamHemlockRelationshipLevel 1
          interludeXpAll (toBonus "bonus" 1)
        _ -> error "invalid codex entry"
      pure s
    ScenarioResolution r -> scope "resolutions" do
      let
        resolution2 = do
          resolution "resolution2"

          -- "Remove 1 [tablet] token from the predation bag. Return any
          -- remaining [tablet] tokens in the predation bag to the chaos bag
          -- for the remainder of the campaign."
          -- TODO add a "predationCleanup" handler in ThePredatoryHouse that
          -- consumes this message, drops one tablet, and pushes AddChaosToken
          -- for the rest.
          predatoryHouse <- selectJust $ storyIs Stories.thePredatoryHouse
          sendMessage predatoryHouse $ ScenarioSpecific "predationCleanup" Null

          -- "If the investigators 'found Little Sylvie,' an investigator may
          -- add the Little Sylvie story asset to their deck."
          whenM (getHasRecord FoundLittleSylvie)
            $ addCampaignCardToDeckChoice_ Assets.littleSylvie

          sealedLocs <- select $ LocationWithToken Resource
          sealCount <- sum <$> traverse locationSealCount sealedLocs
          victoryDisplay <-
            scenarioFieldMap
              ScenarioVictoryDisplay
              (filter ((== EnemyLocationCardType) . toCardType))
          let enemyLocVictory = length victoryDisplay

          when (sealCount >= 8) $ interludeXpAll (toBonus "sealsBonus" 1)
          when (sealCount >= 10) $ interludeXpAll (toBonus "sealsBonus" 1)
          when (enemyLocVictory >= 8) $ interludeXpAll (toBonus "enemyLocationsBonus" 1)
          when (enemyLocVictory >= 10) $ interludeXpAll (toBonus "enemyLocationsBonus" 1)

          when (enemyLocVictory >= 8) do
            judithControlled <-
              selectAny
                $ assetIs Assets.judithParkTheMuscle
                <> AssetControlledBy Anyone
            remodeling <- getHasRecord JudithIsRemodeling
            when (judithControlled && remodeling) do
              incrementRecordCount JudithParkRelationshipLevel 1
              interludeXpAll (toBonus "bonus" 1)

          allGainXp attrs
          record $ AreasSurveyed HemlockHarbor

          -- Routing into the next prelude (per Day/Time) is handled by the
          -- TheFeastOfHemlockVale campaign module's nextStep.
          endOfScenario
      case r of
        NoResolution -> do
          resolution "noResolution"
          push R2
        Resolution 1 -> do
          resolution "resolution1"
          push R2
        Resolution 2 -> resolution2
        _ -> error "invalid resolution"
      pure s
    _ -> HemlockHouse <$> liftRunMessage msg attrs
