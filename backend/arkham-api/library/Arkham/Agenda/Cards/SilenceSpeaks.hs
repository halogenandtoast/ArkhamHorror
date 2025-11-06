module Arkham.Agenda.Cards.SilenceSpeaks (silenceSpeaks) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype SilenceSpeaks = SilenceSpeaks AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silenceSpeaks :: AgendaCard SilenceSpeaks
silenceSpeaks = agenda (1, A) SilenceSpeaks Cards.silenceSpeaks (Static 6)

instance RunMessage SilenceSpeaks where
  runMessage msg a@(SilenceSpeaks attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> SilenceSpeaks <$> liftRunMessage msg attrs
