module Arkham.Treachery.Cards.OfferYouCannotRefuse (offerYouCannotRefuse) where

import Arkham.Card
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Treachery.Import.Lifted

newtype OfferYouCannotRefuse = OfferYouCannotRefuse TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

offerYouCannotRefuse :: TreacheryCard OfferYouCannotRefuse
offerYouCannotRefuse = treachery OfferYouCannotRefuse Cards.offerYouCannotRefuse

instance RunMessage OfferYouCannotRefuse where
  runMessage msg t@(OfferYouCannotRefuse attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasResources <- fieldMap InvestigatorResources (>= 5) iid
      if hasResources
        then loseResources iid attrs 5
        else do
          case toCard attrs of
            EncounterCard _ -> error "should be player card"
            VengeanceCard _ -> error "should be player card"
            PlayerCard pc -> do
              removeCardFromDeckForCampaign iid pc
              addCampaignCardToDeck iid =<< genPlayerCard Treacheries.finePrint
      pure t
    _ -> OfferYouCannotRefuse <$> liftRunMessage msg attrs
