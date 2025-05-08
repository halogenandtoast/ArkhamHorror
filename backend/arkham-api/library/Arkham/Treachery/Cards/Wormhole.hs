module Arkham.Treachery.Cards.Wormhole (wormhole) where

import Arkham.Deck qualified as Deck
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Wormhole = Wormhole TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wormhole :: TreacheryCard Wormhole
wormhole = treachery Wormhole Cards.wormhole

instance RunMessage Wormhole where
  runMessage msg t@(Wormhole attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      discardUntilFirst iid attrs Deck.EncounterDeck (basic #location)
      pure t
    RequestedEncounterCard (isSource attrs -> True) (Just iid) (Just card) -> do
      drawCard iid card
      moveToMatch attrs iid (LocationWithCardId card.id)
      pure t
    _ -> Wormhole <$> liftRunMessage msg attrs
