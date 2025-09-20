module Arkham.Agenda.Cards.TheDescentBegins (theDescentBegins) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Scenarios.TheDepthsOfYoth.Helpers

newtype TheDescentBegins = TheDescentBegins AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDescentBegins :: AgendaCard TheDescentBegins
theDescentBegins = agenda (1, A) TheDescentBegins Cards.theDescentBegins (Static 3)

instance RunMessage TheDescentBegins where
  runMessage msg a@(TheDescentBegins attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      placePursuitEnemies
      advanceAgendaDeck attrs
      pure a
    _ -> TheDescentBegins <$> liftRunMessage msg attrs
