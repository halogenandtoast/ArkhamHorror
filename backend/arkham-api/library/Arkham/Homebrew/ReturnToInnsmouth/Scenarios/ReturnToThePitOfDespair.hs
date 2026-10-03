module Arkham.Homebrew.ReturnToInnsmouth.Scenarios.ReturnToThePitOfDespair (
  returnToThePitOfDespair,
) where

import Arkham.Act.CardDefs.TheInnsmouthConspiracy.ThePitOfDespair qualified as Acts
import Arkham.Agenda.CardDefs.TheInnsmouthConspiracy.ThePitOfDespair qualified as Agendas
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.ThePitOfDespair qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Homebrew.ReturnToInnsmouth.CardDefs.Acts qualified as HBActs
import Arkham.Homebrew.ReturnToInnsmouth.CardDefs.Locations qualified as HBLocations
import Arkham.Homebrew.ReturnToInnsmouth.CardDefs.Treacheries qualified as HBTreacheries
import Arkham.Homebrew.ReturnToInnsmouth.Helpers (scenarioI18n)
import Arkham.Homebrew.ReturnToInnsmouth.Sets qualified as Sets
import Arkham.Key
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.FloodedCaverns qualified as Locations
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.ThePitOfDespair qualified as Locations
import Arkham.Location.Grid
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheInnsmouthConspiracy.ThePitOfDespair
import Arkham.Treachery.CardDefs.TheInnsmouthConspiracy.ThePitOfDespair qualified as Treacheries

newtype ReturnToThePitOfDespair = ReturnToThePitOfDespair ThePitOfDespair
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToThePitOfDespair :: Difficulty -> ReturnToThePitOfDespair
returnToThePitOfDespair difficulty =
  scenarioWith
    (ReturnToThePitOfDespair . ThePitOfDespair)
    ":return-to-innsmouth:018"
    "Return to The Pit of Despair"
    difficulty
    []
    (referenceL .~ "07041")

{- | The setup block is the official The Pit of Despair block, with this box's deltas
declared up front: two replaced encounter sets and the new version of the act card. The
Return to Flooded Caverns swap is a partial one -- one of each tunnel, not the whole set
-- so it is written out below rather than declared as a 'replaceSet'.
-}
instance RunMessage ReturnToThePitOfDespair where
  runMessage msg (ReturnToThePitOfDespair inner@(ThePitOfDespair attrs)) =
    runQueueT $ scenarioI18n "returnToThePitOfDespair" $ case msg of
      Setup -> runScenarioSetup (ReturnToThePitOfDespair . ThePitOfDespair) attrs do
        setIsReturnTo
        replaceSet Set.AgentsOfCthulhu Sets.StalkersOfCthulhu
        replaceSet Set.RisingTide Sets.RollingTide
        substitute Acts.thePit HBActs.thePitV2

        setup $ ul do
          li "gatherSets"
          li "replacedSets"
          li "tidalTunnels"
          li "setAsideTroublingMemories"
          unscoped $ li "shuffleRemainder"

        gather Sets.ReturnToThePitOfDespair
        gather Set.ThePitOfDespair
        gather Set.CreaturesOfTheDeep
        gather Set.FloodedCaverns
        gather Sets.ReturnToFloodedCaverns
        gather Set.RisingTide
        gather Set.ShatteredMemories
        gather Set.AgentsOfCthulhu
        gather Set.Rats

        setAgendaDeck [Agendas.awakening, Agendas.theWaterRises, Agendas.sacrificeForTheDeep]
        setActDeck [Acts.thePit, Acts.theEscape]

        startAt =<< placeInGrid (Pos 0 0) Locations.unfamiliarChamber
        setAside [Locations.idolChamber, Locations.altarToDagon, Locations.sealedExit]

        randomizedKeys <- shuffleM $ map UnrevealedKey [RedKey, YellowKey, PurpleKey]
        setAsideKeys $ BlueKey : GreenKey : randomizedKeys

        -- "Replace one of each Tidal Pool, Underground River and Underwater Cavern from
        -- the original Flooded Caverns set with its counterpart from the Return to
        -- Flooded Caverns", leaving six unique tunnels plus the two scenario locations.
        (inPlayTidalTunnels, tidalTunnelDeck) <-
          splitAt 3
            <$> shuffleM
              [ Locations.boneRiddenPit
              , Locations.fishGraveyard
              , Locations.underwaterCavern
              , HBLocations.underwaterCavern
              , Locations.tidalPool
              , HBLocations.tidalPool
              , Locations.undergroundRiver
              , HBLocations.undergroundRiver
              ]
        addExtraDeck TidalTunnelDeck tidalTunnelDeck
        for_ (zip [Pos (-1) 0, Pos 1 0, Pos 0 (-1)] inPlayTidalTunnels) (uncurry placeInGrid)

        setAside
          [ Enemies.theAmalgam
          , Treacheries.blindsense
          , Treacheries.blindsense
          , Treacheries.fromTheDepths
          , Treacheries.fromTheDepths
          , Treacheries.fromTheDepths
          , HBTreacheries.troublingMemories
          , HBTreacheries.troublingMemories
          ]
      _ -> ReturnToThePitOfDespair <$> liftRunMessage msg inner
