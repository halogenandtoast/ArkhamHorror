module Arkham.Treachery.Cards.Anamnesis (anamnesis) where

import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Anamnesis = Anamnesis TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anamnesis :: TreacheryCard Anamnesis
anamnesis = treachery Anamnesis Cards.anamnesis

instance RunMessage Anamnesis where
  runMessage msg t@(Anamnesis attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOneM iid do
        withI18n $ countVar 2 $ labeledI18n "takeHorror" $ assignHorror iid attrs 2
        withI18n $ countVar 1 $ labeledI18n "placeAgendaDoomCanAdvance"
          $ placeDoomOnAgendaAndCheckAdvance 1
      pure t
    _ -> Anamnesis <$> liftRunMessage msg attrs
