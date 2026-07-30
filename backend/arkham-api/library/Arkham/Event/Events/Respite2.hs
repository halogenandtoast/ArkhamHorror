module Arkham.Event.Events.Respite2 (respite2) where

import Arkham.Card.Id
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype Respite2 = Respite2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

respite2 :: EventCard Respite2
respite2 = event Respite2 Cards.respite2

instance RunMessage Respite2 where
  runMessage msg e@(Respite2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      cards <- select $ inDiscardOf iid <> basic (oneOf [#event, #skill] <> CardWithLevel 0)
      -- The chosen cards are shuffled in as a group, so we only record the
      -- choices here and shuffle once in `Do`, before the draw. Shuffling one
      -- at a time silently does nothing when the deck is empty, since FAQ
      -- (1.13) forbids a *single* card being shuffled into an empty deck.
      focusCards cards $ chooseUpToNM_ iid 3 $ targets cards (handleTarget iid attrs)
      do_ msg
      pure e
    HandleTargetChoice _ (isSource attrs -> True) (CardIdTarget cid) -> do
      let chosen = toResultDefault [] attrs.meta
      pure . Respite2 $ attrs & setMeta (cid : chosen)
    Do (PlayThisEvent iid (is attrs -> True)) -> do
      cards <- traverse fetchCard (toResultDefault @[CardId] [] attrs.meta)
      shuffleCardsIntoDeck iid cards
      drawCards iid attrs 1
      pure e
    _ -> Respite2 <$> liftRunMessage msg attrs
