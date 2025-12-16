module Arkham.Agenda.Cards.ConfluxOfConsequence (confluxOfConsequence) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype ConfluxOfConsequence = ConfluxOfConsequence AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

confluxOfConsequence :: AgendaCard ConfluxOfConsequence
confluxOfConsequence = agenda (1, A) ConfluxOfConsequence Cards.confluxOfConsequence (Static 3)

instance RunMessage ConfluxOfConsequence where
  runMessage msg a@(ConfluxOfConsequence attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> ConfluxOfConsequence <$> liftRunMessage msg attrs
