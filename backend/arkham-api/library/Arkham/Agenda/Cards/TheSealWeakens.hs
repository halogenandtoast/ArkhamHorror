module Arkham.Agenda.Cards.TheSealWeakens (theSealWeakens) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheSealWeakens = TheSealWeakens AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSealWeakens :: AgendaCard TheSealWeakens
theSealWeakens = agenda (1, A) TheSealWeakens Cards.theSealWeakens (Static 12)

instance RunMessage TheSealWeakens where
  runMessage msg a@(TheSealWeakens attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheSealWeakens <$> liftRunMessage msg attrs
