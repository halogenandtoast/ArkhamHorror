module Arkham.Event.Cards.DarkInsight (darkInsight, DarkInsight (..)) where

import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Window (cardDrawn)

newtype DarkInsight = DarkInsight EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkInsight :: EventCard DarkInsight
darkInsight = event DarkInsight Cards.darkInsight

instance RunMessage DarkInsight where
  runMessage msg e@(DarkInsight attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      let card = cardDrawn attrs.windows
      cancelCardDraw attrs card
      case card of
        PlayerCard _ -> push $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [card]
        EncounterCard _ -> push $ ShuffleCardsIntoDeck Deck.EncounterDeck [card]
        VengeanceCard {} -> error "Unhandled"

      pure e
    _ -> DarkInsight <$> liftRunMessage msg attrs
