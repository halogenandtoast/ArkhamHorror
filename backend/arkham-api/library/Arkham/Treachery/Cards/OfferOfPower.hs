module Arkham.Treachery.Cards.OfferOfPower (offerOfPower) where

import Arkham.Campaigns.NightOfTheZealot.Helpers
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype OfferOfPower = OfferOfPower TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

offerOfPower :: TreacheryCard OfferOfPower
offerOfPower = treachery OfferOfPower Cards.offerOfPower

instance RunMessage OfferOfPower where
  runMessage msg t@(OfferOfPower attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOneM iid do
        campaignI18n $ labeled' "offerOfPower.drawAndPlaceDoom" do
          drawCards iid attrs 2
          placeDoomOnAgendaAndCheckAdvance 2
        withI18n $ countVar 2 $ labeled' "takeHorror" $ assignHorror iid attrs 2
      pure t
    _ -> OfferOfPower <$> liftRunMessage msg attrs
