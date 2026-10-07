module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Scenarios.ReturnToThePitOfDespair (
  returnToThePitOfDespair,
) where

import Arkham.Act.CardDefs.TheInnsmouthConspiracy.ThePitOfDespair qualified as Acts
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.FlavorText
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Acts qualified as HBActs
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Locations qualified as HBLocations
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as HBTreacheries
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers (officialSetup, scenarioI18n)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Sets qualified as Sets
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.FloodedCaverns qualified as Locations
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheInnsmouthConspiracy.ThePitOfDespair

newtype ReturnToThePitOfDespair = ReturnToThePitOfDespair ThePitOfDespair
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue, HasModifiersFor)

returnToThePitOfDespair :: Difficulty -> ReturnToThePitOfDespair
returnToThePitOfDespair difficulty =
  scenarioWith
    (ReturnToThePitOfDespair . ThePitOfDespair)
    ":return-to-the-innsmouth-conspiracy:018"
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
          li.nested "gatherSets" do
            li.returnTo "replacedSets"
            li.returnTo "replacedCards"
          officialSetup "thePitOfDespair" do
            li.nested "placeKeys" do
              li "faceupKeys"
              li "facedownKeys"
              li "removeKeys"
            li.nested "placeLocations" do
              li "startAt"
            li "setAsideLocations"
          li.returnTo "tidalTunnels"
          officialSetup "thePitOfDespair" do
            li "tidalTunnels"
            li "setAsideTidalTunnels"
            li "setAsideCards"
          li.returnTo "setAsideTroublingMemories"
          officialSetup "thePitOfDespair" $ li "floodTokens"
          unscoped $ li "shuffleRemainder"

        {- "Replace one of each Tidal Pool, Underground River and Underwater Cavern from
        the original Flooded Caverns set with its counterpart from the Return to Flooded
        Caverns set." -}
        replaceOneOf Locations.tidalPool HBLocations.tidalPool
        replaceOneOf Locations.undergroundRiver HBLocations.undergroundRiver
        replaceOneOf Locations.underwaterCavern HBLocations.underwaterCavern
        gather Sets.ReturnToThePitOfDespair
        gather Sets.ReturnToFloodedCaverns
        setupThePitOfDespair attrs

        setAside $ replicate 2 HBTreacheries.troublingMemories
      _ -> ReturnToThePitOfDespair <$> liftRunMessage msg inner
