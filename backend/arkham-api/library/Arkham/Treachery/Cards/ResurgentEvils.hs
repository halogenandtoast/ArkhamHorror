module Arkham.Treachery.Cards.ResurgentEvils (resurgentEvils) where

import Arkham.I18n
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
    Revelation iid (isSource attrs -> True) -> withI18n do
      chooseOneM iid do
        countVar 2 $ labeled' "drawTopCardOfEncounterDeck" $ drawEncounterCards iid attrs 2
        countVar 1 $ labeled' "placeAgendaDoomCanAdvance" $ placeDoomOnAgendaAndCheckAdvance 1
      pure t
    _ -> ResurgentEvils <$> liftRunMessage msg attrs
