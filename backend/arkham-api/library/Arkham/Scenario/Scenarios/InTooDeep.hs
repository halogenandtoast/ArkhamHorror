module Arkham.Scenario.Scenarios.InTooDeep (InTooDeep (..), inTooDeep) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.EncounterSet qualified as Set
import Arkham.Id
import Arkham.Key
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (metaL)
import Arkham.Scenarios.InTooDeep.Helpers hiding (setBarriers)
import Arkham.Scenarios.InTooDeep.Helpers qualified as Helpers
import Control.Lens (use)

newtype InTooDeep = InTooDeep ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inTooDeep :: Difficulty -> InTooDeep
inTooDeep difficulty = scenario InTooDeep "07123" "In Too Deep" difficulty []

instance HasChaosTokenValue InTooDeep where
  getChaosTokenValue iid tokenFace (InTooDeep attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

setBarriers :: ReverseQueue m => LocationId -> LocationId -> Int -> ScenarioBuilderT m ()
setBarriers a b n = do
  meta <- toResultDefault (Meta mempty) <$> use metaL
  setMeta $ Helpers.setBarriers a b n meta

instance RunMessage InTooDeep where
  runMessage msg s@(InTooDeep attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro"
      pure s
    Setup -> runScenarioSetup InTooDeep attrs do
      setUsesGrid

      gather Set.InTooDeep
      gather Set.CreaturesOfTheDeep
      gather Set.RisingTide
      gather Set.Syzygy
      gather Set.TheLocals
      gather Set.AgentsOfCthulhu

      setAgendaDeck
        [ Agendas.barricadedStreets
        , Agendas.relentlessTide
        , Agendas.floodedStreets
        , Agendas.rageOfTheDeep
        ]
      setActDeck [Acts.throughTheLabyrinth]

      -- bottom row
      desolateCoastline <- placeInGrid (Pos 0 0) Locations.desolateCoastline
      shorewardSlums <- placeInGrid (Pos (-1) 0) Locations.shorewardSlumsInTooDeep
      innsmouthJail <- placeInGrid (Pos (-2) 0) Locations.innsmouthJailInTooDeep
      gilmanHouse <- placeInGrid (Pos (-3) 0) Locations.gilmanHouseInTooDeep
      sawboneAlley <- placeInGrid (Pos (-4) 0) Locations.sawboneAlleyInTooDeep

      -- middle row
      innsmouthHarbour <- placeInGrid (Pos 0 1) Locations.innsmouthHarbourInTooDeep
      fishStreetBridge <- placeInGrid (Pos (-1) 1) Locations.fishStreetBridgeInTooDeep
      innsmouthSquare <- placeInGrid (Pos (-2) 1) Locations.innsmouthSquareInTooDeep
      firstNationalGrocery <- placeInGrid (Pos (-3) 1) Locations.firstNationalGroceryInTooDeep
      theLittleBookshop <- placeInGrid (Pos (-4) 1) Locations.theLittleBookshopInTooDeep

      -- top row
      theHouseOnWaterStreet <- placeInGrid (Pos 0 2) Locations.theHouseOnWaterStreetInTooDeep
      marshRefinery <- placeInGrid (Pos (-1) 2) Locations.marshRefineryInTooDeep
      newChurchGreen <- placeInGrid (Pos (-2) 2) Locations.newChurchGreenInTooDeep
      esotericOrderOfDagon <- placeInGrid (Pos (-3) 2) Locations.esotericOrderOfDagonInTooDeep
      railroadStation <- placeInGrid (Pos (-4) 2) Locations.railroadStation

      -- bottom row
      setBarriers desolateCoastline shorewardSlums 2
      setBarriers shorewardSlums innsmouthJail 1
      setBarriers innsmouthJail gilmanHouse 3
      setBarriers gilmanHouse sawboneAlley 1

      -- middle row
      setBarriers innsmouthHarbour fishStreetBridge 1
      setBarriers fishStreetBridge innsmouthSquare 2
      setBarriers innsmouthSquare firstNationalGrocery 2
      setBarriers firstNationalGrocery theLittleBookshop 2
      setBarriers theLittleBookshop railroadStation 1

      -- top row
      setBarriers theHouseOnWaterStreet marshRefinery 1
      setBarriers marshRefinery newChurchGreen 3
      setBarriers newChurchGreen esotericOrderOfDagon 1
      setBarriers esotericOrderOfDagon railroadStation 4

      startAt desolateCoastline

      setAsideKeys [RedKey, BlueKey, GreenKey, YellowKey, PurpleKey, BlackKey, WhiteKey]
    _ -> InTooDeep <$> liftRunMessage msg attrs
