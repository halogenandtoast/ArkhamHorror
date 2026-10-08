module Arkham.Location.Cards.TheInnsmouthConspiracy.DevilReef.HiddenCove (hiddenCove) where

import Arkham.Ability
import Arkham.Card.CardType
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.DevilReef qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheInnsmouthConspiracy.DevilReef.Helpers (islandCells, out, side)

newtype HiddenCove = HiddenCove LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenCove :: LocationCard HiddenCove
hiddenCove = locationWith HiddenCove Cards.hiddenCove 3 (Static 0) connectsToAdjacent

instance HasAbilities HiddenCove where
  getAbilities (HiddenCove a) =
    extendRevealed a [mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)]

instance RunMessage HiddenCove where
  runMessage msg l@(HiddenCove attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      tunnels <- getScenarioDeck TidalTunnelDeck
      unfathomableDepths <- getSetAsideCardsMatching $ CardWithType LocationType

      -- A tunnel beyond the island, and the depths beyond that.
      (p1, p2) <-
        islandCells attrs.id ([out, out <> side], [out, out <> out]) >>= \case
          [a, b] -> pure (a, b)
          _ -> error "Hidden Cove needs two cells"
      case tunnels of
        [] -> pure ()
        (x : _) -> placeLocationInGrid_ p1 x

      shuffleM unfathomableDepths >>= \case
        [] -> pure ()
        (x : _) -> placeLocationInGrid_ p2 x
      pure l
    _ -> HiddenCove <$> liftRunMessage msg attrs
