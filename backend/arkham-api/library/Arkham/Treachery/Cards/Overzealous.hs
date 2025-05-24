module Arkham.Treachery.Cards.Overzealous (overzealous) where

import Arkham.Card
import Arkham.Deck
import Arkham.Keyword qualified as Keyword
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Overzealous = Overzealous TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overzealous :: TreacheryCard Overzealous
overzealous = treachery Overzealous Cards.overzealous

instance RunMessage Overzealous where
  runMessage msg t@(Overzealous attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      drawEncounterCardsEdit iid attrs 1 (setTarget attrs)
      pure t
    DrewCards iid drewCards | maybe False (isTarget attrs) drewCards.target -> do
      for_ drewCards.cards \card -> do
        cardResolutionModifier card attrs card (AddKeyword Keyword.Surge)
        drawCardFrom iid (toCard card) EncounterDeck
      pure t
    _ -> Overzealous <$> liftRunMessage msg attrs
