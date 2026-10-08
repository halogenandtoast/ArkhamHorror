module Arkham.Location.Cards.TheInnsmouthConspiracy.DevilReef.LonelyIsle (lonelyIsle, LonelyIsle (..)) where

import Arkham.Ability
import Arkham.Helpers.Scenario
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.DevilReef qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheInnsmouthConspiracy.DevilReef.Helpers (islandCells, otherSide, out, side)

newtype LonelyIsle = LonelyIsle LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lonelyIsle :: LocationCard LonelyIsle
lonelyIsle = locationWith LonelyIsle Cards.lonelyIsle 5 (Static 0) connectsToAdjacent

instance HasAbilities LonelyIsle where
  getAbilities (LonelyIsle a) =
    extendRevealed a [mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)]

instance RunMessage LonelyIsle where
  runMessage msg l@(LonelyIsle attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      tunnels <- take 2 <$> getScenarioDeck TidalTunnelDeck
      -- A tunnel to either side of the island.
      positions <- islandCells attrs.id ([otherSide, side], [out, side])
      zipWithM_ placeLocationInGrid positions tunnels
      pure l
    _ -> LonelyIsle <$> liftRunMessage msg attrs
