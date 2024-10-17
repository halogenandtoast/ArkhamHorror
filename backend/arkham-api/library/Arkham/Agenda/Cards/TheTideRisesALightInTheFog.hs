module Arkham.Agenda.Cards.TheTideRisesALightInTheFog (
  TheTideRisesALightInTheFog (..),
  theTideRisesALightInTheFog,
) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheTideRisesALightInTheFog = TheTideRisesALightInTheFog AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTideRisesALightInTheFog :: AgendaCard TheTideRisesALightInTheFog
theTideRisesALightInTheFog = agenda (3, A) TheTideRisesALightInTheFog Cards.theTideRisesALightInTheFog (Static 10)

instance RunMessage TheTideRisesALightInTheFog where
  runMessage msg a@(TheTideRisesALightInTheFog attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheTideRisesALightInTheFog <$> liftRunMessage msg attrs
