module Arkham.Treachery.Cards.OfferYouCannotRefuse (
  offerYouCannotRefuse,
  OfferYouCannotRefuse (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Treachery.Runner

newtype OfferYouCannotRefuse = OfferYouCannotRefuse TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

offerYouCannotRefuse :: TreacheryCard OfferYouCannotRefuse
offerYouCannotRefuse = treachery OfferYouCannotRefuse Cards.offerYouCannotRefuse

instance RunMessage OfferYouCannotRefuse where
  runMessage msg t@(OfferYouCannotRefuse attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasResources <- fieldMap InvestigatorResources (>= 5) iid
      if hasResources
        then push $ LoseResources iid (toSource attrs) 5
        else do
          case toCard attrs of
            EncounterCard _ -> error "should be player card"
            VengeanceCard _ -> error "should be player card"
            PlayerCard pc -> do
              finePrint <- genPlayerCard Treacheries.finePrint
              pushAll
                [ RemoveCardFromDeckForCampaign iid pc
                , AddCardToDeckForCampaign iid finePrint
                ]
      pure t
    _ -> OfferYouCannotRefuse <$> runMessage msg attrs
