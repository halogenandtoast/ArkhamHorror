module Arkham.Agenda.Cards.HiddenEntanglements (hiddenEntanglements) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype HiddenEntanglements = HiddenEntanglements AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenEntanglements :: AgendaCard HiddenEntanglements
hiddenEntanglements = agenda (3, A) HiddenEntanglements Cards.hiddenEntanglements (Static 6)

instance HasAbilities HiddenEntanglements where
  getAbilities (HiddenEntanglements attrs) = [mkAbility attrs 1 $ ActionAbility [#resign] (ActionCost 1)]

instance RunMessage HiddenEntanglements where
  runMessage msg a@(HiddenEntanglements attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resign iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sufferMentalTrauma iid 1
        investigatorDefeated attrs iid
      pure a
    _ -> HiddenEntanglements <$> liftRunMessage msg attrs
