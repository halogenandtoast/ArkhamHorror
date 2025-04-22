module Arkham.Treachery.Cards.StarsOfHyades (starsOfHyades) where

import Arkham.Card
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype StarsOfHyades = StarsOfHyades TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

starsOfHyades :: TreacheryCard StarsOfHyades
starsOfHyades = treachery StarsOfHyades Cards.starsOfHyades

instance RunMessage StarsOfHyades where
  runMessage msg t@(StarsOfHyades attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      events <- fieldMap InvestigatorCardsUnderneath (filterCards (card_ #event)) iid
      case nonEmpty events of
        Nothing -> assignDamageAndHorror iid attrs 1 1
        Just xs -> do
          deckSize <- fieldMap InvestigatorDeck length iid
          discardedEvent <- sample xs
          chooseOneM iid do
            targeting (toCardId discardedEvent) do
              push $ RemoveFromGame (CardIdTarget $ toCardId discardedEvent)
          when (deckSize < 5) $ shuffleIntoDeck iid attrs
      pure t
    _ -> StarsOfHyades <$> liftRunMessage msg attrs
