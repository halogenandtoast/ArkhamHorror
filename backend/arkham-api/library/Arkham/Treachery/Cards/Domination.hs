module Arkham.Treachery.Cards.Domination (domination) where

import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCards)
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Domination = Domination TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

domination :: TreacheryCard Domination
domination = treachery Domination Cards.domination

instance RunMessage Domination where
  runMessage msg t@(Domination attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOneM iid $ withI18n do
        countVar 1
          $ labeled' "placeAgendaDoomCanAdvance"
          $ placeDoomOnAgendaAndCheckAdvance 1
        countVar 2 $ labeled' "eachInvestigatorDiscardsCardsFromHand" do
          eachInvestigator \iid' -> chooseAndDiscardCards iid' attrs 2
        countVar 1 $ labeled' "eachInvestigatorLosesActions" do
          eachInvestigator \iid' -> loseActions iid' attrs 1
      pure t
    _ -> Domination <$> liftRunMessage msg attrs
