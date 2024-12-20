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
      case toCard attrs of
        VengeanceCard _ -> error "not a vengeance card"
        EncounterCard _ -> error "not an encounter card"
        PlayerCard pc -> do
          assignDamageAndHorror iid attrs 2 2
          placeDoomOnAgendaAndCheckAdvance 1
          removeCardFromDeckForCampaign iid pc
          addCampaignCardToDeck iid =<< genPlayerCard Events.darkPact
          removeTreachery attrs
          pure t
    _ -> ThePriceOfFailure <$> liftRunMessage msg attrs
