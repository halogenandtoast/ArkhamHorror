module Arkham.Agenda.Cards.UndergroundSurvey (undergroundSurvey) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype UndergroundSurvey = UndergroundSurvey AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undergroundSurvey :: AgendaCard UndergroundSurvey
undergroundSurvey = agenda (1, A) UndergroundSurvey Cards.undergroundSurvey (Static 12)

instance RunMessage UndergroundSurvey where
  runMessage msg a@(UndergroundSurvey attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> UndergroundSurvey <$> liftRunMessage msg attrs
