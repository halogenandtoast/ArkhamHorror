module Arkham.Scenario.Scenarios.HemlockHouse (hemlockHouse) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Attack (enemyAttack)
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.EnemyLocation.Types (enemyLocationAsEnemyId)
import {-# SOURCE #-} Arkham.Game.Utils (maybeEnemyLocation)
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Query (getLead)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Message.Lifted.Story (resolveStoryWithPlacement)
import Arkham.Placement
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.HemlockHouse.Helpers
import Arkham.Story.Cards qualified as Stories

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
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro1"
      pure s
    Setup -> runScenarioSetup HemlockHouse attrs do
      setUsesGrid

      gather Set.HemlockHouse
      gather Set.AgentsOfTheColour
      gather Set.Blight
      gather Set.Fire
      gather Set.Transfiguration
      gather Set.LockedDoors
      gather Set.Rats

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

      placeInGrid_ (Pos (-1) 0) Locations.parlorHemlockHouse
      foyer <- placeInGrid (Pos 0 0) Locations.foyerHemlockHouse
      placeInGrid_ (Pos 1 0) Locations.diningRoomHemlockHouse

      bedroom <- placeInGrid (Pos 0 3) topBedroom

      for_ (zip [Pos x y | x <- [-1 .. 1], y <- [1, 2]] locations) (uncurry placeInGrid_)

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

      lead <- getLead
      thePredatoryHouse <- genCard Stories.thePredatoryHouse
      resolveStoryWithPlacement lead thePredatoryHouse Global
    ScenarioSpecific "enemyLocationDefeated" (maybeResult -> Just lid) -> do
      grid <- getGrid
      lead <- getLead
      investigators <- select $ InvestigatorAt (LocationWithId lid)
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
    _ -> HemlockHouse <$> liftRunMessage msg attrs
