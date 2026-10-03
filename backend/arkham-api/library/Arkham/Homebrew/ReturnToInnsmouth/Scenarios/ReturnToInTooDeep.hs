module Arkham.Homebrew.ReturnToInnsmouth.Scenarios.ReturnToInTooDeep (returnToInTooDeep) where

import Arkham.Act.CardDefs.TheInnsmouthConspiracy.InTooDeep qualified as Acts
import Arkham.Agenda.CardDefs.TheInnsmouthConspiracy.InTooDeep qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Campaigns.TheInnsmouthConspiracy.Key
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.InTooDeep qualified as Enemies
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.TheVanishingOfElinaHarper qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Homebrew.ReturnToInnsmouth.CardDefs.Acts qualified as HBActs
import Arkham.Homebrew.ReturnToInnsmouth.CardDefs.Enemies qualified as HBEnemies
import Arkham.Homebrew.ReturnToInnsmouth.CardDefs.Treacheries qualified as HBTreacheries
import Arkham.Homebrew.ReturnToInnsmouth.Helpers (scenarioI18n)
import Arkham.Homebrew.ReturnToInnsmouth.Sets qualified as Sets
import Arkham.Id
import Arkham.Key
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.InTooDeep qualified as Locations
import Arkham.Location.Grid
import Arkham.Message.Lifted.Log
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheInnsmouthConspiracy.InTooDeep
import Arkham.Scenarios.TheInnsmouthConspiracy.InTooDeep.Helpers hiding (scenarioI18n, setBarriers)
import Arkham.Scenarios.TheInnsmouthConspiracy.InTooDeep.Helpers qualified as Helpers
import Control.Lens (use)

newtype ReturnToInTooDeep = ReturnToInTooDeep InTooDeep
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToInTooDeep :: Difficulty -> ReturnToInTooDeep
returnToInTooDeep difficulty =
  scenarioWith
    (ReturnToInTooDeep . InTooDeep)
    ":return-to-innsmouth:028"
    "Return to In Too Deep"
    difficulty
    []
    (referenceL .~ "07123")

{- | The official scenario keeps its barrier counts in the scenario's own meta; this is
its local helper, which is not exported.
-}
setBarriers' :: ReverseQueue m => LocationId -> LocationId -> Int -> ScenarioBuilderT m ()
setBarriers' a b n = do
  meta <- toResultDefault (Meta mempty) <$> use (attrsL . metaL)
  setMeta $ Helpers.setBarriers a b n meta

instance RunMessage ReturnToInTooDeep where
  runMessage msg (ReturnToInTooDeep inner@(InTooDeep attrs)) =
    runQueueT $ scenarioI18n "returnToInTooDeep" $ case msg of
      Setup -> runScenarioSetup (ReturnToInTooDeep . InTooDeep) attrs do
        setIsReturnTo
        replaceSet Set.AgentsOfCthulhu Sets.StalkersOfCthulhu
        replaceSet Set.RisingTide Sets.RollingTide
        replaceSet Set.Syzygy Sets.Occultation
        substitute Acts.throughTheLabyrinth HBActs.throughTheLabyrinthV2

        setUsesGrid

        setup $ ul do
          li "gatherSets"
          li "replacedSets"
          li "setAsideInnsmouthInfluence"
          unscoped $ li "shuffleRemainder"

        gather Sets.ReturnToInTooDeep
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
        gilmanHouse <- placeInGrid (Pos (-3) 0) Locations.gilmanHouse
        sawboneAlley <- placeInGrid (Pos (-4) 0) Locations.sawboneAlleyInTooDeep

        -- middle row
        innsmouthHarbour <- placeInGrid (Pos 0 1) Locations.innsmouthHarbourInTooDeep
        fishStreetBridge <- placeInGrid (Pos (-1) 1) Locations.fishStreetBridge
        innsmouthSquare <- placeInGrid (Pos (-2) 1) Locations.innsmouthSquare
        firstNationalGrocery <- placeInGrid (Pos (-3) 1) Locations.firstNationalGrocery
        theLittleBookshop <- placeInGrid (Pos (-4) 1) Locations.theLittleBookshop

        -- top row
        theHouseOnWaterStreet <- placeInGrid (Pos 0 2) Locations.theHouseOnWaterStreetInTooDeep
        marshRefinery <- placeInGrid (Pos (-1) 2) Locations.marshRefinery
        newChurchGreen <- placeInGrid (Pos (-2) 2) Locations.newChurchGreenInTooDeep
        esotericOrderOfDagon <- placeInGrid (Pos (-3) 2) Locations.esotericOrderOfDagonInTooDeep
        railroadStation <- placeInGrid (Pos (-4) 2) Locations.railroadStation

        -- bottom row
        setBarriers' desolateCoastline shorewardSlums 2
        setBarriers' shorewardSlums innsmouthJail 1
        setBarriers' innsmouthJail gilmanHouse 3
        setBarriers' gilmanHouse sawboneAlley 1

        -- middle row
        setBarriers' innsmouthHarbour fishStreetBridge 1
        setBarriers' fishStreetBridge innsmouthSquare 2
        setBarriers' innsmouthSquare firstNationalGrocery 2
        setBarriers' firstNationalGrocery theLittleBookshop 2
        setBarriers' theLittleBookshop railroadStation 1

        -- top row
        setBarriers' theHouseOnWaterStreet marshRefinery 1
        setBarriers' marshRefinery newChurchGreen 3
        setBarriers' newChurchGreen esotericOrderOfDagon 1
        setBarriers' esotericOrderOfDagon railroadStation 4

        startAt desolateCoastline

        setAsideKeys $ map UnrevealedKey [RedKey, BlueKey, GreenKey, YellowKey, PurpleKey, WhiteKey]

        mHideout <- maybeResult <$$> getCircledRecord PossibleHideouts
        for_ (join mHideout) \hideout -> do
          let
            hideoutLocation = case hideout of
              InnsmouthJail -> innsmouthJail
              ShorewardSlums -> shorewardSlums
              SawboneAlley -> sawboneAlley
              TheHouseOnWaterStreet -> theHouseOnWaterStreet
              EsotericOrderOfDagon -> esotericOrderOfDagon
              NewChurchGreen -> newChurchGreen
          placeKey hideoutLocation BlackKey

        outForBlood <- mapMaybe (maybeResult <=< unrecorded) <$> getRecordSet OutForBlood
        for_ outForBlood \case
          BrianBurnham -> enemyAt_ Enemies.brianBurnhamWantsOut firstNationalGrocery
          BarnabasMarsh -> enemyAt_ Enemies.barnabasMarshTheChangeIsUponHim marshRefinery
          OtheraGilman -> enemyAt_ Enemies.otheraGilmanProprietessOfTheHotel gilmanHouse
          ZadokAllen -> enemyAt_ Enemies.zadokAllenDrunkAndDisorderly fishStreetBridge
          JoyceLittle -> enemyAt_ Enemies.joyceLittleBookshopOwner theLittleBookshop
          RobertFriendly -> enemyAt_ Enemies.robertFriendlyDisgruntledDockworker innsmouthHarbour

        setAside
          [ Enemies.ravagerFromTheDeep
          , Enemies.ravagerFromTheDeep
          , -- Stalkers of Cthulhu replaces Agents of Cthulhu, so its matching cards stand in
            -- for the Young Deep Ones the official setup sets aside (FAQ v2.0, and the
            -- designer's own worked example).
            HBEnemies.deepOneAmbusher
          , HBEnemies.deepOneAmbusher
          , Assets.joeSargentRattletrapBusDriver
          , Assets.teachingsOfTheOrder
          , Enemies.innsmouthShoggoth
          , Enemies.angryMob
          , HBTreacheries.innsmouthInfluence
          , HBTreacheries.innsmouthInfluence
          , HBTreacheries.innsmouthInfluence
          , HBTreacheries.innsmouthInfluence
          ]

        for_ [theHouseOnWaterStreet, innsmouthHarbour, desolateCoastline] (push . IncreaseFloodLevel)
      _ -> ReturnToInTooDeep <$> liftRunMessage msg inner
