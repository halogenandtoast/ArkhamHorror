module Arkham.Scenario.Scenarios.HemlockHouse (hemlockHouse) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetLocation))
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Campaigns.TheFeastOfHemlockVale.Key
import Arkham.Card
import Arkham.Effect.Window
import Arkham.EncounterSet qualified as Set
import Arkham.EnemyLocation.Cards qualified as EnemyLocations
import Arkham.EnemyLocation.Types (enemyLocationAsEnemyId)
import {-# SOURCE #-} Arkham.Game.Utils (maybeEnemyLocation)
import Arkham.Helpers.Agenda (whenCurrentAgendaStepIs)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (MetaModifier))
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Xp
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Message (pattern AfterSkillTest)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (incrementRecordCount, record, remember, remembered)
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (Field (ScenarioVictoryDisplay))
import Arkham.ScenarioLogKey
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
    Skull -> do
      mLid <- field InvestigatorLocation iid
      floor' <- maybe (pure 0) getFloorNumber mLid
      pure $ toChaosTokenValue attrs Skull (max 1 floor') (max 2 (floor' * 2))
    Cultist -> pure $ toChaosTokenValue attrs Cultist 1 2
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 5
    otherFace -> getChaosTokenValue iid otherFace attrs

{- FOURMOLU_DISABLE -}
standaloneChaosBag :: [ChaosTokenFace]
standaloneChaosBag =
  [ #"+1" , #"0" , #"0" , #"-1" , #"-1" , #"-2" , #"-2" , #"-3" , #"-3" , #"-5"
  , Cultist , Cultist , Tablet , Tablet , ElderThing , Skull
  ]
{- FOURMOLU_ENABLE -}

instance RunMessage HemlockHouse where
  runMessage msg s@(HemlockHouse attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      setChaosTokens standaloneChaosBag
      pure s
    PreScenarioSetup -> scope "intro" do
      day <- getCampaignDay
      time <- getCampaignTime
      let isNight = time == Night
      flavor do
        setTitle "title"
        p.basic "body"
        ul $ li.nested.validate isNight "nightSkip" do
          li.validate (not isNight && day == Day1) "day1"
          li.validate (not isNight && day == Day2) "day2"
          li.validate (not isNight && day == Day3) "day3"
      case (day, time) of
        (Day1, Day) -> story $ i18nWithTitle "intro1"
        (Day2, Day) -> story $ i18nWithTitle "intro2"
        (Day3, Day) -> story $ i18nWithTitle "intro3"
        _ -> story $ i18nWithTitle "intro4"
      pure s
    Setup -> runScenarioSetup HemlockHouse attrs do
      setScenarioDayAndTime
      day <- getCampaignDay
      time <- getCampaignTime

      let
        useV1 = day == Day1 || (time == Day && day == Day3)
        agenda2 = if useV1 then Agendas.theHouseStirsV2 else Agendas.theHouseStirsV1

      setup $ ul do
        li "gatherSets"
        li "currentDaySet"
        li "currentDayMarker"
        li "againstTheHouse"
        li.validate useV1 "houseStirsV1"
        li.validate (not useV1) "houseStirsV2"
        li.nested "locations" do
          li "firstFloor"
          li "shuffleFloors"
          li "topBedroom"
          li "removeRemaining"
          li "startAtFoyer"
          li "startAtBedroom"
        li.nested.validate (time == Day) "residents" do
          if time == Day
            then do
              li.validate (day == Day1) "gideonAndSylvie"
              li.validate (day == Day1 || day == Day2) "william"
              li.validate (day == Day2 || day == Day3) "judith"
              li.validate (day == Day3) "theo"
              li "removeResidents"
            else do
              li "gideonAndSylvie"
              li "william"
              li "judith"
              li "theo"
              li "removeResidents"
        li "predatoryHouse"
        li "setOutOfPlay"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      scope "enemyLocations" $ flavor do
        setTitle "title"
        p "body"
        ul do
          li "noMove"
          li "keepEntities"
          li.nested "sealed" do
            li "unsealed"
          li.nested "defeat" do
            li "noAbove"
            li "middleEmpty"

      setUsesGrid

      gather Set.HemlockHouse
      gather Set.AgentsOfTheColour
      gather Set.Blight
      gather Set.Fire
      gather Set.Transfiguration
      gather Set.LockedDoors
      gather Set.Rats

      gatherAndSetAside Set.Residents

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

      placedFloors23 <-
        for (zip [Pos x y | x <- [-1 .. 1], y <- [1, 2]] locations) \(pos, def) -> do
          lid <- placeInGrid pos def
          pure (def, lid, pos)

      setAside [EnemyLocations.shapelessCellar]

      void $ fromGathered #location

      setupHemlockDay day time

      startAt $ if day == Day3 then bedroom else foyer

      setAside [Acts.againstTheHouse]
      setAsideWith
        ( \case
            EncounterCard ec -> do
              let flipped = EncounterCard ec {ecIsFlipped = Just True}
              replaceCard ec.id flipped
              pure flipped
            c -> pure c
        )
        [Stories.thePredatoryHouse]
      setAsideEvery $ mapOneOf cardIs [Treacheries.fire, Treacheries.outOfTheWalls, Treacheries.pulledIn]
      unless (day == Day1 && time == Day) $ setAside [Assets.littleSylvie]

      when (time == Day) $ case day of
        Day1 -> do
          assetAt_ Assets.gideonMizrahSeasonedSailor parlor
          assetAt_ Assets.williamHemlockAspiringPoet bedroom
          let bedroomCodes :: Set CardCode =
                setFromList
                  [ Locations.bedroomHemlockHouse32.cardCode
                  , Locations.bedroomHemlockHouse33.cardCode
                  , Locations.bedroomHemlockHouse34.cardCode
                  , Locations.bedroomHemlockHouse35.cardCode
                  ]
              placedBedrooms =
                [ (lid, y)
                | (def, lid, Pos _ y) <- placedFloors23
                , toCardCode def `member` bedroomCodes
                ]
              lowestBedrooms = case sortOn snd placedBedrooms of
                [] -> [bedroom]
                sorted@((_, lowestY) : _) -> [lid | (lid, y) <- sorted, y == lowestY]
          case lowestBedrooms of
            [x] -> assetAt_ Assets.littleSylvie x
            xs -> do
              lead <- getLead
              chooseOneM lead $ for_ xs \lid ->
                targeting lid $ createAssetAt_ Assets.littleSylvie (AtLocation lid)
        Day2 -> do
          assetAt_ Assets.judithParkTheMuscle parlor
          assetAt_ Assets.williamHemlockAspiringPoet bedroom
        Day3 -> do
          assetAt_ Assets.judithParkTheMuscle parlor
          assetAt_ Assets.theoPetersJackOfAllTrades foyer
    ResolveChaosToken _ Cultist iid | isEasyStandard attrs -> do
      whenJustM (field InvestigatorLocation iid) \lid ->
        whenJustM (maybeEnemyLocation lid) $ exhaustEnemy attrs . enemyLocationAsEnemyId
      pure s
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ _
      | token.face == Cultist
      , isHardExpert attrs -> do
          whenJustM (field InvestigatorLocation iid) \lid ->
            whenJustM (maybeEnemyLocation lid) $ exhaustEnemy attrs . enemyLocationAsEnemyId
          pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ | token.face == Tablet -> do
      n <- if isHardExpert attrs then perPlayer 1 else pure 1
      whenJustM (nearestEnemyLocationTo iid) \lid ->
        whenJustM (maybeEnemyLocation lid) \el ->
          healDamage (enemyLocationAsEnemyId el) attrs n
      pure s
    AfterSkillTest (FailedSkillTest _ _ _ (ChaosTokenTarget token) _ _) | token.face == ElderThing -> do
      whenCurrentAgendaStepIs (`elem` [2, 3]) do
        removeChaosToken ElderThing
        predatoryHouse <- selectJust $ storyIs Stories.thePredatoryHouse
        sendMessage predatoryHouse $ AddChaosToken ElderThing
      pure s
    AfterSkillTest (PassedSkillTest _ _ _ (ChaosTokenTarget token) _ _)
      | token.face == ElderThing
      , isHardExpert attrs -> do
          whenCurrentAgendaStepIs (`elem` [2, 3]) do
            removeChaosToken ElderThing
            predatoryHouse <- selectJust $ storyIs Stories.thePredatoryHouse
            sendMessage predatoryHouse $ AddChaosToken ElderThing
          pure s
    ScenarioSpecific "enemyLocationDefeated" (maybeResult -> Just lid) -> do
      grid <- getGrid
      lead <- getLead
      investigators <-
        filterM (\iid -> (== Just lid) <$> field InvestigatorLocation iid)
          =<< select UneliminatedInvestigator
      enemies <- select $ EnemyAt (LocationWithId lid)
      storyAssets <- select $ AssetAt (LocationWithId lid) <> StoryAsset <> UncontrolledAsset
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
    ScenarioSpecific "codex" v -> scope "codex" do
      let (iid :: InvestigatorId, source :: Source, n :: Int) = toResult v
      let entry x = scope x $ flavor $ setTitle "title" >> p.green "body"
      day <- getCampaignDay
      case n of
        4 -> do
          william <- selectJust $ assetIs Assets.williamHemlockAspiringPoet
          judith <- selectOne $ assetIs Assets.judithParkTheMuscle
          controlsSylvie <-
            selectAny $ assetIs Assets.littleSylvie <> AssetControlledBy (InvestigatorWithId iid)
          williamLoc <- field AssetLocation william
          sameLoc <- case (judith, williamLoc) of
            (Just jid, Just wloc) -> (== Just wloc) <$> field AssetLocation jid
            _ -> pure False
          let day1FoundSylvie = day == Day1 && controlsSylvie
              day1Otherwise = day == Day1 && not controlsSylvie
              day2Argument = day == Day2 && sameLoc
              day2Otherwise = day == Day2 && not sameLoc
          scope "william" $ flavor do
            setTitle "title"
            compose.green do
              p.validate day1FoundSylvie "day1FoundSylvie"
              hr
              p.validate day1Otherwise "day1Otherwise"
              hr
              p.validate day2Argument "day2Argument"
              hr
              p.validate day2Otherwise "day2Otherwise"
          case day of
            Day1 -> do
              if controlsSylvie
                then do
                  takeControlOfAsset iid william
                  remember FoundLittleSylvie
                  incrementRecordCount WilliamHemlockRelationshipLevel 1
                  interludeXpAll (toBonus "bonus" 1)
                else do
                  codexFinished 4
                  eachInvestigator \iid' -> gainClues iid' source 1
                  sylvie <- selectJust $ assetIs Assets.littleSylvie
                  createAbilityEffect EffectGameWindow
                    $ restricted (SourceableWithCardCode Assets.littleSylvie sylvie) 3 OnSameLocation actionAbility
                  createAbilityEffect EffectGameWindow
                    $ skillTestAbility
                    $ onlyOnce
                    $ restricted
                      (SourceableWithCardCode Assets.williamHemlockAspiringPoet william)
                      1
                      (OnSameLocation <> you (ControlsAsset (assetIs Assets.littleSylvie)))
                      parleyAction_
            Day2 ->
              if sameLoc
                then scenarioSpecific "codex" (iid, source, Sigma)
                else do
                  takeControlOfAsset iid william
                  createAbilityEffect EffectGameWindow
                    $ skillTestAbility
                    $ onlyOnce
                    $ restricted
                      (SourceableWithCardCode Assets.williamHemlockAspiringPoet william)
                      1
                      ( OnSameLocation
                          <> exists
                            ( assetIs Assets.williamHemlockAspiringPoet
                                <> AssetAt (LocationWithAsset $ assetIs Assets.judithParkTheMuscle)
                            )
                      )
                      parleyAction_
            Day3 -> pure ()
        6 -> do
          helping <- remembered YouAreHelpingGideon
          scope "gideon" $ flavor do
            setTitle "title"
            compose.green do
              p.validate helping "helping"
              hr
              p.validate (not helping) "otherwise"
          if helping
            then do
              codexFinished 6
              incrementRecordCount GideonMizrahRelationshipLevel 1
              interludeXpAll (toBonus "bonus" 1)
            else do
              remember YouAreHelpingGideon
              selectOne (storyIs Stories.thePredatoryHouse) >>= \case
                Just predatoryHouse ->
                  sendMessage predatoryHouse $ ScenarioSpecific "cancelNextPredation" Null
                Nothing ->
                  gameModifier attrs (StoryTarget $ StoryId Stories.thePredatoryHouse.cardCode)
                    $ MetaModifier
                    $ object ["cancelNextPredation" .= True]
        7 -> do
          judith <- selectJust $ assetIs Assets.judithParkTheMuscle
          william <- selectOne $ assetIs Assets.williamHemlockAspiringPoet
          judithLoc <- field AssetLocation judith
          sameLoc <- case (william, judithLoc) of
            (Just wid, Just jloc) -> (== Just jloc) <$> field AssetLocation wid
            _ -> pure False
          let day2Argument = day == Day2 && sameLoc
              day2Otherwise = day == Day2 && not sameLoc
              day3 = day == Day3
          scope "judith" $ flavor do
            setTitle "title"
            compose.green do
              p.validate day2Argument "day2Argument"
              hr
              p.validate day2Otherwise "day2Otherwise"
              hr
              p.validate day3 "day3"
          case day of
            Day2 ->
              if sameLoc
                then scenarioSpecific "codex" (iid, source, Sigma)
                else do
                  codexFinished 7
                  takeControlOfAsset iid judith
                  createAbilityEffect EffectGameWindow
                    $ skillTestAbility
                    $ onlyOnce
                    $ restricted
                      (SourceableWithCardCode Assets.judithParkTheMuscle judith)
                      1
                      ( OnSameLocation
                          <> exists
                            ( assetIs Assets.judithParkTheMuscle
                                <> AssetAt (LocationWithAsset $ assetIs Assets.williamHemlockAspiringPoet)
                            )
                      )
                      (parleyAction $ ResourceCost 2)
            Day3 -> do
              takeControlOfAsset iid judith
              remember JudithIsRemodeling
            _ -> pure ()
        8 -> do
          codexFinished 8
          entry "theo"
          theo <- selectJust $ assetIs Assets.theoPetersJackOfAllTrades
          takeControlOfAsset iid theo
        Theta -> do
          codexFinished Theta
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
      case r of
        NoResolution -> do
          resolution "noResolution"
          push R2
        Resolution 1 -> do
          resolution "resolution1"
          push R2
        Resolution 2 -> do
          resolution "resolution2"

          -- "Remove 1 [elder_thing] token from the predation bag. Return any
          -- remaining [elder_thing] tokens in the predation bag to the chaos bag
          -- for the remainder of the campaign."
          -- If The Predatory House never entered play, there is no predation
          -- bag to clean up.
          whenJustM (selectOne $ storyIs Stories.thePredatoryHouse) \predatoryHouse ->
            sendMessage predatoryHouse $ ScenarioSpecific "predationCleanup" Null

          whenM (remembered FoundLittleSylvie) $ addCampaignCardToDeckChoice_ Assets.littleSylvie

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
            judithControlled <- selectAny $ assetIs Assets.judithParkTheMuscle <> AssetControlledBy Anyone
            remodeling <- remembered JudithIsRemodeling
            when (judithControlled && remodeling) do
              incrementRecordCount JudithParkRelationshipLevel 1
              interludeXpAll (toBonus "bonus" 1)

          allGainXp attrs
          record $ AreasSurveyed HemlockHarbor

          endOfScenario
        _ -> error "invalid resolution"
      pure s
    _ -> HemlockHouse <$> liftRunMessage msg attrs
