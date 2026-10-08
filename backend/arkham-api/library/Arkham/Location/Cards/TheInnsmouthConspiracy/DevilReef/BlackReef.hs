module Arkham.Location.Cards.TheInnsmouthConspiracy.DevilReef.BlackReef (blackReef) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Card.CardType
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.DevilReef qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Matcher qualified as Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheInnsmouthConspiracy.DevilReef.Helpers (islandCells, out, side)

newtype BlackReef = BlackReef LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackReef :: LocationCard BlackReef
blackReef = locationWith BlackReef Cards.blackReef 2 (Static 0) connectsToAdjacent

instance HasAbilities BlackReef where
  getAbilities (BlackReef a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ Matcher.RevealLocation #after Anyone (be a)

instance RunMessage BlackReef where
  runMessage msg l@(BlackReef attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      increaseThisFloodLevel attrs
      tunnels <- take 2 <$> getScenarioDeck TidalTunnelDeck

      -- Two tunnels alongside and beyond, and the depths in the corner past them.
      (tunnelCells, depths) <-
        splitAt 2 <$> islandCells attrs.id ([side, out, out <> side], [out, side, out <> side])
      zipWithM_ placeLocationInGrid tunnelCells tunnels
      unfathomableDepths <- getSetAsideCardsMatching $ CardWithType LocationType
      zipWithM_ placeLocationInGrid_ depths =<< shuffleM unfathomableDepths
      pure l
    _ -> BlackReef <$> liftRunMessage msg attrs
