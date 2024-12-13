module Arkham.Agenda.Cards.ManifestationsOfEvil ( manifestationsOfEvil) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype ManifestationsOfEvil = ManifestationsOfEvil AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

manifestationsOfEvil :: AgendaCard ManifestationsOfEvil
manifestationsOfEvil = agenda (7, A) ManifestationsOfEvil Cards.manifestationsOfEvil (Static 2)

instance RunMessage ManifestationsOfEvil where
  runMessage msg a@(ManifestationsOfEvil attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> ManifestationsOfEvil <$> liftRunMessage msg attrs
