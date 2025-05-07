module Arkham.Agenda.Cards.OutOfTime (outOfTime) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype OutOfTime = OutOfTime AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outOfTime :: AgendaCard OutOfTime
outOfTime = agenda (5, A) OutOfTime Cards.outOfTime (Static 3)

instance RunMessage OutOfTime where
  runMessage msg a@(OutOfTime attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        investigatorDefeated attrs iid
        sufferMentalTrauma iid 1
      push R2
      pure a
    _ -> OutOfTime <$> liftRunMessage msg attrs
