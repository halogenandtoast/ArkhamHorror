module Arkham.Scenario.Scenarios.HemlockHouse (hemlockHouse) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.EncounterSet qualified as Set
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
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

      placeStory Stories.thePredatoryHouse
    _ -> HemlockHouse <$> liftRunMessage msg attrs
