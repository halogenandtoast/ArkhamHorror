module Arkham.Agenda.Cards.DarknessClosesIn (darknessClosesIn) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype DarknessClosesIn = DarknessClosesIn AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darknessClosesIn :: AgendaCard DarknessClosesIn
darknessClosesIn = agenda (2, A) DarknessClosesIn Cards.darknessClosesIn (Static 12)

instance RunMessage DarknessClosesIn where
  runMessage msg a@(DarknessClosesIn attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sufferMentalTrauma iid 1
        investigatorDefeated attrs iid
      pure a
    _ -> DarknessClosesIn <$> liftRunMessage msg attrs
