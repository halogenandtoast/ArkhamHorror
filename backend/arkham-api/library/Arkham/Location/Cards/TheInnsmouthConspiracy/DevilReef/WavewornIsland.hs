module Arkham.Location.Cards.TheInnsmouthConspiracy.DevilReef.WavewornIsland (wavewornIsland) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Card.CardType
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.DevilReef qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheInnsmouthConspiracy.DevilReef.Helpers (
  back,
  islandCells,
  otherSide,
  out,
  side,
 )

newtype WavewornIsland = WavewornIsland LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wavewornIsland :: LocationCard WavewornIsland
wavewornIsland =
  locationWith WavewornIsland Cards.wavewornIsland 4 (Static 0) connectsToAdjacent

instance HasAbilities WavewornIsland where
  getAbilities (WavewornIsland a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)

instance RunMessage WavewornIsland where
  runMessage msg l@(WavewornIsland attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      increaseThisFloodLevel attrs
      tunnels <- take 2 <$> getScenarioDeck TidalTunnelDeck
      unfathomableDepths <- getSetAsideCardsMatching $ CardWithType LocationType

      -- A tunnel to either side, and the depths past the island.
      (tunnelCells, depths) <-
        splitAt 2 <$> islandCells attrs.id ([otherSide, side, out], [out, side, back])
      zipWithM_ placeLocationInGrid tunnelCells tunnels

      zipWithM_ placeLocationInGrid_ depths =<< shuffleM unfathomableDepths
      pure l
    _ -> WavewornIsland <$> liftRunMessage msg attrs
