module Arkham.Treachery.Cards.TheSignOfHastur (theSignOfHastur) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TheSignOfHastur = TheSignOfHastur TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSignOfHastur :: TreacheryCard TheSignOfHastur
theSignOfHastur = treachery TheSignOfHastur Cards.theSignOfHastur

instance HasAbilities TheSignOfHastur where
  getAbilities (TheSignOfHastur attrs) = case attrs.placement of
    AttachedToAct _ ->
      [mkAbility attrs 1 $ forced $ InvestigatorWouldTakeHorror #when You AnySource]
    AttachedToAgenda _ ->
      [mkAbility attrs 1 $ forced $ InvestigatorWouldTakeHorror #when You AnySource]
    _ -> []

instance RunMessage TheSignOfHastur where
  runMessage msg t@(TheSignOfHastur attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      acts <- select $ NotAct $ ActWithTreachery $ treacheryIs Cards.theSignOfHastur
      agendas <- select $ NotAgenda $ AgendaWithTreachery $ treacheryIs Cards.theSignOfHastur
      chooseOneM iid do
        targets acts $ attachTreachery attrs
        targets agendas $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      dealAdditionalHorror iid 1 []
      pure t
    _ -> TheSignOfHastur <$> liftRunMessage msg attrs
