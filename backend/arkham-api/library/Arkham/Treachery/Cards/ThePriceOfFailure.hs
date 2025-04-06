module Arkham.Treachery.Cards.ThePriceOfFailure (thePriceOfFailure) where

import Arkham.Card
import Arkham.Event.Cards qualified as Events
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ThePriceOfFailure = ThePriceOfFailure TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePriceOfFailure :: TreacheryCard ThePriceOfFailure
thePriceOfFailure = treachery ThePriceOfFailure Cards.thePriceOfFailure

instance RunMessage ThePriceOfFailure where
  runMessage msg t@(ThePriceOfFailure attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      for_ (preview _PlayerCard attrs.card) \pc -> do
        assignDamageAndHorror iid attrs 2 2
        placeDoomOnAgendaAndCheckAdvance 1
        removeCardFromDeckForCampaign iid pc
        darkPact <- genPlayerCard Events.darkPact
        addCampaignCardToDeck iid DoNotShuffleIn darkPact
        addToDiscard iid (only darkPact)
        removeTreachery attrs
      pure t
    _ -> ThePriceOfFailure <$> liftRunMessage msg attrs
