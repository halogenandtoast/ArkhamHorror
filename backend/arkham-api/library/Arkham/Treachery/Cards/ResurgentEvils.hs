module Arkham.Treachery.Cards.ResurgentEvils (resurgentEvils) where

import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ResurgentEvils = ResurgentEvils TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

resurgentEvils :: TreacheryCard ResurgentEvils
resurgentEvils = treachery ResurgentEvils Cards.resurgentEvils

instance RunMessage ResurgentEvils where
  runMessage msg t@(ResurgentEvils attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOneM iid do
        labeled "Draw the top 2 cards of the encounter deck." do
          drawEncounterCards iid attrs 2
        labeled "Place 1 doom on the current agenda. This effect can cause the current agenda to advance." do
          placeDoomOnAgendaAndCheckAdvance 1
      pure t
    _ -> ResurgentEvils <$> liftRunMessage msg attrs
