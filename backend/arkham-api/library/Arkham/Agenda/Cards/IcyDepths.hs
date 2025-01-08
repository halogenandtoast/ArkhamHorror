module Arkham.Agenda.Cards.IcyDepths (icyDepths) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype IcyDepths = IcyDepths AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

icyDepths :: AgendaCard IcyDepths
icyDepths = agenda (8, A) IcyDepths Cards.icyDepths (Static 6)

instance RunMessage IcyDepths where
  runMessage msg a@(IcyDepths attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sufferMentalTrauma iid 1
        investigatorDefeated attrs iid
      pure a
    _ -> IcyDepths <$> liftRunMessage msg attrs
