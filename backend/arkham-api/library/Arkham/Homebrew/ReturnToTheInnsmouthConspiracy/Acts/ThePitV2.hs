module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Acts.ThePitV2 (thePitV2) where

import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheInnsmouthConspiracy.Memory
import Arkham.Direction
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.ThePitOfDespair qualified as Enemies
import Arkham.Helpers.Scenario
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as HBTreacheries
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.ThePitOfDespair qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheInnsmouthConspiracy.ThePitOfDespair.Helpers
import Arkham.Treachery.CardDefs.TheInnsmouthConspiracy.ThePitOfDespair qualified as Treacheries

newtype ThePitV2 = ThePitV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

thePitV2 :: ActCard ThePitV2
thePitV2 = act (1, A) ThePitV2 Cards.thePitV2 (groupClueCost $ PerPlayer 3)

{- | Differs from The Pit (07045) on the back: Troubling Memories is shuffled in too,
the encounter discard pile goes back in with it, and the Tidal Tunnel deck is formed
from the set-aside scenario-specific locations as well as the tunnels.
-}
instance RunMessage ThePitV2 where
  runMessage msg a@(ThePitV2 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      createSetAsideEnemy_ Enemies.theAmalgam =<< getLead
      shuffleSetAsideIntoEncounterDeck
        $ mapOneOf
          cardIs
          [ HBTreacheries.troublingMemories
          , Treacheries.blindsense
          , Treacheries.fromTheDepths
          ]
      shuffleEncounterDiscardBackIn
      shuffleSetAsideIntoScenarioDeck TidalTunnelDeck
        $ oneOf
          [ CardWithTitle "Tidal Tunnel"
          , mapOneOf cardIs [Locations.idolChamber, Locations.altarToDagon, Locations.sealedExit]
          ]
      doStep 1 msg
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      grid <- getGrid
      let
        locationPositions lid = case findInGrid lid grid of
          Nothing -> []
          Just pos -> emptyPositionsInDirections grid pos [GridDown, GridLeft, GridRight]

      positions <- nub . concatMap locationPositions <$> select RevealedLocation
      tidalTunnelDeck <- getScenarioDeck TidalTunnelDeck
      for_ (zip positions tidalTunnelDeck) (uncurry placeLocationInGrid)

      flashback Flashback1
      recoverMemory AMeetingWithThomasDawson
      advanceActDeck attrs
      pure a
    _ -> ThePitV2 <$> liftRunMessage msg attrs
