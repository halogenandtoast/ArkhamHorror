module Arkham.Agenda.Cards.EmpireOfTheUndead (empireOfTheUndead) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype EmpireOfTheUndead = EmpireOfTheUndead AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

empireOfTheUndead :: AgendaCard EmpireOfTheUndead
empireOfTheUndead = agenda (2, A) EmpireOfTheUndead Cards.empireOfTheUndead (Static 12)

instance RunMessage EmpireOfTheUndead where
  runMessage msg a@(EmpireOfTheUndead attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        investigatorDefeated attrs iid
        sufferPhysicalTrauma iid 1
      pure a
    _ -> EmpireOfTheUndead <$> liftRunMessage msg attrs
