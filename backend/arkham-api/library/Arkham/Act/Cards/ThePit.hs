module Arkham.Act.Cards.ThePit (ThePit (..), thePit) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheInnsmouthConspiracy.Memory
import Arkham.Direction
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Scenario
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePitOfDespair.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype ThePit = ThePit ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

thePit :: ActCard ThePit
thePit = act (1, A) ThePit Cards.thePit (groupClueCost $ PerPlayer 3)

instance RunMessage ThePit where
  runMessage msg a@(ThePit attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      createSetAsideEnemy_ Enemies.theAmalgam =<< getLead
      shuffleSetAsideIntoEncounterDeck
        $ mapOneOf cardIs [Treacheries.blindsense, Treacheries.fromTheDepths]

      shuffleSetAsideIntoScenarioDeck TidalTunnelDeck $ CardWithTitle "Tidal Tunnel"
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
    _ -> ThePit <$> liftRunMessage msg attrs
