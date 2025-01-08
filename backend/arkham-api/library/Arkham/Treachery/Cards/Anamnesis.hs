module Arkham.Treachery.Cards.Anamnesis (anamnesis) where

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
        labeled "Take 2 horror" $ assignHorror iid attrs 2
        labeled "Place 1 doom on the current agenda. This effect can cause the current agenda to advance."
          $ placeDoomOnAgendaAndCheckAdvance 1
      pure t
    _ -> Anamnesis <$> liftRunMessage msg attrs
